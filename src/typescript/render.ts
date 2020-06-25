import { DCons } from "@thi.ng/dcons"
import * as g from "@thi.ng/geom"
import { closestPoint, withAttribs, Line } from "@thi.ng/geom"
import { IHiccupShape, Type } from "@thi.ng/geom-api"
import { walk } from "@thi.ng/hdom-canvas"
import { TAU } from "@thi.ng/math"
import {
  concat,
  invert23,
  Mat23Like,
  mulV23,
  transform23,
  translation23
} from "@thi.ng/matrices"
import { add2, Vec, Vec2Like } from "@thi.ng/vectors"
import {
  arcStrokeWidth,
  connectionWidth,
  constantInputStroke,
  nodeBackgroundOpacity,
  nodeBackgrounds,
  nodeOutputRadius,
  nodeRadius
} from "./constants"
import { isPressed, MouseButtons } from "./mouse"
import { refreshInputArcsImpl } from "./sync"
import { getMouseTarget, MouseTarget, MouseTargetKind } from "./target"
import type { ForeignAction, ForeignActionConfig } from "./types/ForeignAction"
import {
  GeometryCache,
  IHasNode,
  InputPartialConnection,
  NodeGeometry,
  NodeId,
  PartialKind
} from "./types/Node"

// Used in the Default purescript implementation of GeomCache
export const emptyGeometryCache = (): GeometryCache => ({
  nodes: new Map(),
  camera: transform23(null, [0, 0], 0, 1) as Mat23Like,
  selectedOutput: null,
  selectedInput: null,
  selectedNode: null,
  dragging: null,
  selectedNodes: new Set(),
  zOrder: new DCons(),
  connection: { _type: PartialKind.Nothing },
  connectionPreview: g.line([0, 0], [0, 0], {
    weight: connectionWidth
  })
})

// TODO: go trough every piece of rendering code and
// make sure everything is floored before rendering.
// It's unbelievable how much this helps in terms of performance :D

/**
 * Create the geometry for a node input.
 *
 * @param position The position of the input
 */
const dottedInput = (position: Vec2Like) => {
  const attribs = {
    stroke: constantInputStroke,
    weight: arcStrokeWidth.normal,
    lineCap: "butt",
    dash: [(Math.PI * nodeRadius) / 20]
  }

  return g.circle(position, nodeRadius, attribs)
}

const updateConnectionPreview = (cache: GeometryCache, mouse: Vec) => {
  if (cache.connection._type === PartialKind.Nothing) {
    return
  }

  cache.connectionPreview.points[1] = mouse

  if (
    cache.connection._type === PartialKind.Output &&
    cache.connection.node.output
  ) {
    cache.connectionPreview.attribs!.stroke = cache.connection.node.output.attribs!.fill
    cache.connectionPreview.points[0] = cache.connection.node.position
  } else if (
    cache.connection._type === PartialKind.Input &&
    cache.connection.node.lastState
  ) {
    const state = cache.connection.node.lastState

    // We point this to our custom id ("mouse")
    cache.connection.node.inputOverwrites = {
      [cache.connection.index]: "mouse" as NodeId
    }

    refreshInputArcsImpl(cache, cache.connection.id, state, {
      mouse
    })

    cache.connection.node.inputOverwrites = {}

    cache.connectionPreview.attribs!.stroke = cache.connection.geom.attribs!.stroke
    cache.connectionPreview.points[0] = closestPoint(
      cache.connection.geom,
      mouse
    )!
  }
}

/**
 * Create the geometry for a node input.
 *
 * @param position The position of the node.
 */
const createInputGeometry = (position: Vec2Like) => {
  const attribs = {
    stroke: "black",
    weight: arcStrokeWidth.normal,
    selectable: true,
    oldColor: null
  }

  const arc = g.arc(position, nodeRadius, 0, 0, TAU)

  return withAttribs(arc, attribs)
}

/**
 * Generate an empty node geometry.
 *
 * @param position The position of the node
 * @param numberOfInputs The number of input arcs to create.
 * @param hasOutput If this is true a geometry for the output will be generated as well
 */
export const createNodeGeometry = (
  position: Vec2Like,
  numberOfInputs: number,
  hasOutput: boolean
): NodeGeometry => {
  const inputGeom =
    numberOfInputs === 0
      ? [dottedInput(position)]
      : Array(numberOfInputs)
          .fill(1)
          .map(() => createInputGeometry(position))

  const output = hasOutput
    ? g.circle(position, 10, { fill: "yellow", oldColor: null })
    : null

  const background = g.withAttribs(g.circle(position, nodeRadius), {
    alpha: nodeBackgroundOpacity
  })

  return {
    inputs: inputGeom,
    output,
    background,
    position,
    lastState: null,
    inputOverwrites: {},
    connections: Array(numberOfInputs)
      .fill(1)
      .map(() =>
        g.line([0, 0], [0, 0], {
          connected: false,
          weight: arcStrokeWidth.normal
        })
      )
  }
}

/**
 * Returns a transform matrix moving the canvas by half it's size
 *
 * @param ctx The canvas rendering context to get the middle of.
 * @param cache The cache to get the camera from.
 */
const getTransform = (ctx: CanvasRenderingContext2D, cache: GeometryCache) => {
  const transform = transform23(
    null,
    [ctx.canvas.width / 2, ctx.canvas.height / 2],
    0,
    1
  )

  concat(null, transform, cache.camera)

  return transform
}

/**
 * Returns a transform matrix moving the mouse to world coordinates
 *
 * @param ctx The canvas rendering context
 * @param cache The cache to use the camera from
 */
const getMouseTransform = (
  ctx: CanvasRenderingContext2D,
  cache: GeometryCache
) => {
  const bounds = ctx.canvas.getBoundingClientRect()

  const transform = transform23(
    null,
    [-bounds.left - bounds.width / 2, -bounds.height / 2],
    0,
    1
  )

  concat(null, transform, invert23([], cache.camera)!)

  return transform
}

export const renderScene = (
  ctx: CanvasRenderingContext2D,
  cache: GeometryCache
) => {
  ctx.resetTransform()
  ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)

  const matrix = getTransform(ctx, cache)

  const nodes: IHiccupShape[] = [...cache.zOrder]
    .map((id) => cache.nodes.get(id)!)
    .flatMap(({ inputs, output, background, connections, lastState }) => [
      ...(background.attribs!.fill ? [background] : []),
      ...inputs.flatMap((input, index): IHiccupShape[] => {
        if (input.type === Type.CIRCLE) return [input]

        const arc = g.pathFromCubics(g.asCubic(input))
        const connection = connections[index]

        if (connection.attribs!.connected) {
          return [connection, arc]
        }

        return [arc]
      }),
      ...(output === null ? [] : [output])
    ])

  const withPreview =
    cache.connection._type === PartialKind.Nothing
      ? nodes
      : nodes.concat([cache.connectionPreview])

  const shapes = g.group({ transform: matrix }, withPreview).toHiccup()

  walk(ctx, [shapes], {
    attribs: {},
    edits: []
  })
}

/**
 * Selects a particular node in a geometry.
 *
 * @param cache The cache to mutate.
 * @param node The geometry of the node to select
 * @param id THe id of the node to select
 */
const selectNode = (cache: GeometryCache, node: NodeGeometry, id: NodeId) => {
  cache.selectedNodes.add(id)

  if (cache.selectedNode === node) {
    cache.selectedNode = null
  }

  const old = cache.zOrder.find(id)

  if (old) {
    cache.zOrder = cache.zOrder.remove(old)
  }

  cache.zOrder.push(id)

  node.background.attribs!.fill = nodeBackgrounds.selected
}

/**
 * Unselect a particular node from a geometry.
 *
 * @param cache The cache to mutate
 * @param node THe geometry of the node to unselect
 * @param id THe id of the node to unselect
 */
const unselectNode = (cache: GeometryCache, node: NodeGeometry, id: NodeId) => {
  cache.selectedNodes.delete(id)

  node.background.attribs!.fill = undefined
}

/**
 * Reset the color of a specific attribute on a shape to it's old state.
 *
 * @param shape The shape to mutate.
 * @param attr The attribute to reset.
 */
const resetColor = (shape: IHiccupShape, attr = "fill") => {
  if (shape.attribs!.oldColor !== null) {
    shape.attribs![attr] = shape.attribs!.oldColor
    shape.attribs!.oldColor = null
  }
}

/**
 * Returns the brightness of all unconnectable pins to normal.
 *
 * @param cache The cache to mutate.
 */
const resetUnconnectableColor = (cache: GeometryCache) => {
  // TODO: move this to some sort of callback when I need to ask in a modal about stuff.
  if (cache.connection._type === PartialKind.Nothing) return
  else if (cache.connection._type === PartialKind.Output) {
    for (const { id, index } of cache.connection.unconnectable) {
      resetColor(cache.nodes.get(id)!.inputs[index], "stroke")
    }
  } else if (cache.connection._type === PartialKind.Input) {
    for (const id of cache.connection.unconnectable) {
      resetColor(cache.nodes.get(id)!.output!)
    }
  }
}

/**
 * Connect 2 pins together
 *
 * @param config The config for the action to return
 * @param cache The cache to mutate.
 * @param output The output pin of the connection.
 * @param input The input pin of the connection
 */
const createConnection = (
  config: ForeignActionConfig,
  cache: GeometryCache,
  output: IHasNode,
  input: Omit<InputPartialConnection, "unconnectable">
): ForeignAction => {
  resetUnconnectableColor(cache)

  cache.connection._type = PartialKind.Nothing

  return config.createConnection(output.id, input.id, input.index)
}

/**
 * Select the output of the current connection.
 *
 * @param config The config for the action to return
 * @param cache The cache to mutate.
 * @param output The output to select.
 * @param mouse The current mouse position.
 */
const selectOutput = (
  config: ForeignActionConfig,
  cache: GeometryCache,
  output: IHasNode,
  mouse: Vec
): ForeignAction => {
  if (cache.connection._type === PartialKind.Input) {
    if (cache.connection.unconnectable.has(output.id)) {
      return config.nothing
    }

    return createConnection(config, cache, output, cache.connection)
  }

  cache.connection = {
    ...output,
    unconnectable: new Set(),
    _type: PartialKind.Output
  }

  updateConnectionPreview(cache, mouse)

  return config.selectOutput(output.id)
}

/**
 * Select the input of the current connection.
 *
 * @param config The config for the action to return
 * @param cache The cache to mutate.
 * @param input The input to select.
 * @param mouse The current mouse position.
 */
const selectInput = (
  config: ForeignActionConfig,
  cache: GeometryCache,
  input: Omit<InputPartialConnection, "unconnectable">,
  mouse: Vec
): ForeignAction => {
  if (cache.connection._type === PartialKind.Output) {
    // TODO: there's no point in using sets here so we can just plain arrays
    for (const { index, id } of cache.connection.unconnectable) {
      if (index === input.index && id === input.id) {
        return config.nothing
      }
    }

    return createConnection(config, cache, cache.connection, input)
  }

  cache.connection = {
    ...input,
    unconnectable: new Set(),
    _type: PartialKind.Input
  }

  updateConnectionPreview(cache, mouse)

  return config.selectInput(input.id, input.index)
}

/**
 * Move a single node by a certain offset
 *
 * @param geom The geometry to move
 * @param offset The offset to move the geometry by.
 */
const moveNode = (geom: NodeGeometry, offset: Vec) => {
  add2(null, geom.position, offset)
}

/**
 * Move a all selected nodes by some offset.
 *
 * @param cache The cache to mutate.
 * @param offset Amount to move.
 */
const moveNodes = (cache: GeometryCache, offset: Vec) => {
  for (const id of cache.selectedNodes) {
    moveNode(cache.nodes.get(id)!, offset)
  }

  for (const [id, node] of cache.nodes.entries()) {
    if (
      node.lastState &&
      ((cache.selectedNodes.has(id) &&
        node.lastState.inputs.some((id) => id !== null)) ||
        node.lastState.inputs.some(
          (id) => id !== null && cache.selectedNodes.has(id)
        ))
    )
      refreshInputArcsImpl(cache, id, node.lastState)
  }
}

const pan = (cache: GeometryCache, offset: Vec) => {
  concat(null, cache.camera, translation23([], offset))
}

/**
 * Handle a mouseUp event
 *
 * @param config The config for what we want to return
 * @param ctx The context to re-render to.
 * @param event The event to handle.
 * @param cache The cache to mutate.
 */
export const onMouseUp = (
  config: ForeignActionConfig,
  ctx: CanvasRenderingContext2D,
  event: MouseEvent,
  cache: GeometryCache
) => (): ForeignAction => {
  for (const [id, node] of cache.nodes) {
    unselectNode(cache, node, id)
  }

  cache.dragging = null

  return onMouseMove(config, ctx, event, cache)()
}

/**
 * Handle a mouseDown event
 *
 * @param config The config for what we want to return
 * @param ctx The context to re-render to.
 * @param event The event to handle.
 * @param cache The cache to mutate.
 */
export const onMouseDown = (
  config: ForeignActionConfig,
  ctx: CanvasRenderingContext2D,
  event: MouseEvent,
  cache: GeometryCache
) => (): ForeignAction => {
  let action = config.nothing

  const mouse = [event.pageX, event.pageY]
  const transform = getMouseTransform(ctx, cache)
  const mousePosition = mulV23(null, transform, mouse)

  const target = getMouseTarget(mousePosition, cache)

  if (target._type === MouseTargetKind.Node) {
    selectNode(cache, target.node, target.id)
  }

  if (target._type === MouseTargetKind.Nothing) {
    if (
      cache.connection._type === PartialKind.Input &&
      cache.connection.node.lastState
    ) {
      refreshInputArcsImpl(
        cache,
        cache.connection.id,
        cache.connection.node.lastState
      )
    }

    resetUnconnectableColor(cache)

    cache.connection._type = PartialKind.Nothing
  }

  if (target._type === MouseTargetKind.NodeInput) {
    action = selectInput(config, cache, target, mousePosition)
  } else if (target._type === MouseTargetKind.NodeOutput) {
    action = selectOutput(config, cache, target, mousePosition)
  } else {
    cache.dragging = target
  }

  renderScene(ctx, cache)

  return action
}

/**
 * Handle a mouseMove event
 *
 * @param config The config for what we want to return
 * @param ctx The context to re-render to.
 * @param event The event to handle.
 * @param cache The cache to mutate.
 */
export const onMouseMove = (
  config: ForeignActionConfig,
  ctx: CanvasRenderingContext2D,
  event: MouseEvent,
  cache: GeometryCache
) => (): ForeignAction => {
  let target: MouseTarget

  const mouse = [event.pageX, event.pageY]
  const transform = getMouseTransform(ctx, cache)
  const mousePosition = mulV23(null, transform, mouse)

  if (cache.dragging) {
    target = cache.dragging
  } else {
    target = getMouseTarget(mousePosition, cache)
  }

  const pressed = isPressed(event.buttons)

  // On hover effects for outputs
  if (event.buttons === 0) {
    if (
      target._type === MouseTargetKind.NodeOutput &&
      cache.selectedOutput !== target.node
    ) {
      if (cache.selectedOutput) {
        cache.selectedOutput.output.r = nodeOutputRadius.normal
      }

      cache.selectedOutput = target.node
      target.node!.output.r = nodeOutputRadius.onHover
    }

    // On hover effect for inputs
    if (
      target._type === MouseTargetKind.NodeInput &&
      cache.selectedInput !== target.geom
    ) {
      if (cache.selectedInput) {
        cache.selectedInput.attribs!.weight = arcStrokeWidth.normal
      }

      cache.selectedInput = target.geom
      target.geom.attribs!.weight = arcStrokeWidth.onHover
    }

    // On hover effects for nodes
    if (
      target._type === MouseTargetKind.Node &&
      cache.selectedNode !== target.node
    ) {
      if (cache.selectedNode) {
        cache.selectedNode.background.attribs!.fill = undefined
      }

      if (target.node.background.attribs!.fill === undefined) {
        cache.selectedNode = target.node
        target.node.background.attribs!.fill = nodeBackgrounds.onHover
      }
    }
  }

  // Clear old data from the cache
  if (target._type !== MouseTargetKind.NodeOutput && cache.selectedOutput) {
    cache.selectedOutput.output.r = nodeOutputRadius.normal
    cache.selectedOutput = null
  }

  if (target._type !== MouseTargetKind.NodeInput && cache.selectedInput) {
    cache.selectedInput.attribs!.weight = arcStrokeWidth.normal
    cache.selectedInput = null
  }

  if (
    target._type !== MouseTargetKind.Node &&
    cache.selectedNode &&
    cache.selectedNode.background.attribs!.fill === nodeBackgrounds.onHover
  ) {
    cache.selectedNode.background.attribs!.fill = undefined
    cache.selectedNode = null
  }

  const mouseScreenOffset = [event.movementX, event.movementY]

  // Handle dragging nodes
  if (pressed(MouseButtons.LeftButton)) {
    moveNodes(cache, mouseScreenOffset)
  }

  // Handle panning
  if (pressed(MouseButtons.RightButton)) {
    pan(cache, mouseScreenOffset)
  }

  updateConnectionPreview(cache, mousePosition)
  renderScene(ctx, cache)

  return config.nothing
}

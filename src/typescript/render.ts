import * as g from "@thi.ng/geom"
import { walk } from "@thi.ng/hdom-canvas"
import {
  Mat23Like,
  transform23,
  mulV23,
  concat,
  translation23,
  invert23
} from "@thi.ng/matrices"
import type { GeometryCache, NodeId, NodeGeometry } from "./types/Node"
import { Vec2Like, Vec, add2 } from "@thi.ng/vectors"
import {
  nodeRadius,
  arcStrokeWidth,
  constantInputStroke,
  nodeOutputRadius,
  nodeBackgroundOpacity,
  nodeBackgrounds
} from "./constants"
import { TAU } from "@thi.ng/math"
import { Type } from "@thi.ng/geom-api"
import { withAttribs } from "@thi.ng/geom"
import { isPressed, MouseButtons } from "./mouse"
import { DCons } from "@thi.ng/dcons"
import { getMouseTarget, MouseTargetKind, MouseTarget } from "./target"

// Used in the Default purescript implementation of GeomCache
export const emptyGeometryCache: GeometryCache = {
  nodes: new Map(),
  camera: transform23(null, [0, 0], 0, 1) as Mat23Like,
  selectedOutput: null,
  selectedInput: null,
  selectedNode: null,
  dragging: null,
  selectedNodes: new Set(),
  zOrder: new DCons()
}
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

/**
 * Create the geometry for a node input.
 *
 * @param position The position of the input
 * @param step The layer the input lays on.
 * @param input The input-specific data
 */
const renderInput = (position: Vec2Like) => {
  const attribs = {
    stroke: "black",
    weight: arcStrokeWidth.normal,
    selectable: true
  }

  const arc = g.arc(position, nodeRadius, 0, 0, TAU)

  return withAttribs(arc, attribs)
}

export const createNodeGeometry = (
  position: Vec2Like,
  numberOfInputs: number
): NodeGeometry => {
  const inputGeom =
    numberOfInputs === 0
      ? [dottedInput(position)]
      : Array(numberOfInputs)
          .fill(1)
          .map(() => renderInput(position))

  const output = g.circle(position, 10, { fill: "yellow" })

  const background = g.withAttribs(g.circle(position, nodeRadius), {
    alpha: nodeBackgroundOpacity
  })

  return { inputs: inputGeom, output, background, position }
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

  const nodes = [...cache.zOrder]
    .map((id) => cache.nodes.get(id)!)
    .flatMap(({ inputs, output, background }) => [
      ...(background.attribs!.fill ? [background] : []),
      ...inputs.map((input) =>
        input.type === Type.CIRCLE ? input : g.pathFromCubics(g.asCubic(input))
      ),
      output
    ])

  const shapes = g.group({ transform: matrix }, nodes).toHiccup()

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
 * Handle a mouseUp event
 *
 * @param ctx The context to re-render to.
 */
export const onMouseUp = (ctx: CanvasRenderingContext2D) => (
  event: MouseEvent
) => (cache: GeometryCache) => () => {
  for (const [id, node] of cache.nodes) {
    unselectNode(cache, node, id)
  }

  cache.dragging = null

  renderScene(ctx, cache)
}

/**
 * Handle a mouseDown event
 *
 * @param ctx The context to re-render to.
 */
export const onMouseDown = (ctx: CanvasRenderingContext2D) => (
  event: MouseEvent
) => (cache: GeometryCache) => () => {
  const mouse = [event.pageX, event.pageY]
  const transform = getMouseTransform(ctx, cache)
  const mousePosition = mulV23(null, transform, mouse)

  const target = getMouseTarget(mousePosition, cache)

  if (target._type === MouseTargetKind.Node) {
    selectNode(cache, target.node, target.id)
  }

  if (
    target._type === MouseTargetKind.Node ||
    target._type === MouseTargetKind.Nothing
  ) {
    cache.dragging = target
  }

  renderScene(ctx, cache)
}
/**
 * Move a single node by a certain offset
 *
 * @param geom The geometry to move
 * @param offset The offset to move the geometry by.
 */
export const moveNode = (geom: NodeGeometry, offset: Vec) => {
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
}

const pan = (cache: GeometryCache, offset: Vec) => {
  concat(null, cache.camera, translation23([], offset))
}

/**
 * Handle a mouseMove event
 *
 * @param ctx The context to re-render to.
 */
export const onMouseMove = (ctx: CanvasRenderingContext2D) => (
  event: MouseEvent
) => (cache: GeometryCache) => () => {
  let target: MouseTarget

  if (cache.dragging) {
    target = cache.dragging
  } else {
    const mouse = [event.pageX, event.pageY]
    const transform = getMouseTransform(ctx, cache)
    const mousePosition = mulV23(null, transform, mouse)

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

  renderScene(ctx, cache)
}

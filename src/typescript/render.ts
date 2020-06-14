import * as g from "@thi.ng/geom"
import { walk } from "@thi.ng/hdom-canvas"
import {
  Mat23Like,
  transform23,
  mulV23,
  invert,
  viewport,
  invert23,
  scale23,
  concat
} from "@thi.ng/matrices"
import type {
  GeometryCache,
  NodeData,
  NodeId,
  NodeGeometry
} from "./types/Node"
import { Vec2Like, distSq2, Vec, dist } from "@thi.ng/vectors"
import * as Arc from "./arcs"
import {
  nodeRadius,
  inputLayerOffset,
  arcStrokeWidth,
  arcSpacing,
  constantInputStroke,
  nodeOutputRadius,
  pickDistance
} from "./constants"
import { TAU } from "@thi.ng/math"
import { isPressed, MouseButtons } from "./mouse"
import { ADT } from "ts-adt"
import { Type, IHiccupShape } from "@thi.ng/geom-api"
import { closestPoint, withAttribs } from "@thi.ng/geom"

// Used in the Default purescript implementation of GeomCache
export const emptyGeometryCache: GeometryCache = {
  nodes: new Map(),
  camera: transform23(null, [0, 0], 0, 1) as Mat23Like,
  selectedOutput: null,
  selectedInput: null
}
/**
 * Create the geometry for a node input.
 *
 * @param position The position of the input
 */
export const dottedInput = (position: Vec2Like) => {
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
export const renderInput = (
  position: Vec2Like,
  step: number,
  input: Arc.InputWithArc
) => {
  const attribs = {
    stroke: input.color,
    weight: arcStrokeWidth.normal,
    selectable: true
  }

  const spacing = input.isCircle ? 0 : arcSpacing
  const radius = nodeRadius + step * inputLayerOffset

  if (input.isCircle) {
    return g.circle(position, radius, attribs)
  }

  const arc = g.arc(
    position,
    radius,
    0,
    input.arc[0] + spacing,
    input.arc[1] - spacing + TAU * Number(input.arc[1] < input.arc[0])
  )

  return withAttribs(arc, attribs)
}

export const renderNode = (
  getData: (id: NodeId) => Vec2Like,
  node: NodeData
): NodeGeometry => {
  const inputs = Arc.placeInputs(getData, node)

  const inputGeom =
    inputs[0].length === 0
      ? [dottedInput(node.position)]
      : inputs.flatMap((arr, index) =>
          arr.map((input) => renderInput(node.position, index, input))
        )

  const output = g.circle(node.position, 10, { fill: "yellow" })

  return { inputs: inputGeom, output, selected: false }
}

/**
 * Returns a transform matrix moving the canvas by half it's size
 *
 * @param ctx The canvas rendering context to get the middle of.
 */
const getTransform = (ctx: CanvasRenderingContext2D) => {
  return transform23(null, [ctx.canvas.width / 2, ctx.canvas.height / 2], 0, 1)
}

/**
 * Returns a transform matrix moving the mouse to world coordinates
 *
 * @param ctx The canvas rendering context
 */
const getMouseTransform = (ctx: CanvasRenderingContext2D) => {
  const bounds = ctx.canvas.getBoundingClientRect()

  return transform23(
    null,
    [-bounds.left - bounds.width / 2, -bounds.height / 2],
    0,
    1
  )
}

export const renderScene = (
  ctx: CanvasRenderingContext2D,
  cache: GeometryCache
) => {
  ctx.resetTransform()
  ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)

  const matrix = getTransform(ctx)

  const nodes = [...cache.nodes.values()].flatMap(({ inputs, output }) => [
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

interface IHasNode {
  node: NodeGeometry
}

type MouseTarget = ADT<{
  nodeInput: IHasNode & { index: number; geom: g.Arc }
  node: IHasNode
  nodeOutput: IHasNode
  nothing: {}
}>

/**
 * find the smallest element in an array based on a certain criteria
 *
 * @param isSmaller The compare function
 * @param arr The array to search trough
 * @param def Fallback in case of empty arrays
 */
const minBy = <T>(isSmaller: (a: T, b: T) => boolean, arr: T[], def: T) =>
  arr.reduce((acc, curr) => (isSmaller(curr, acc) ? curr : acc), def)

/**
 * Finds the object in the scene the mouse is hovering over
 *
 * @param mousePosition The position the mouse is at
 * @param cache The geometry cache to search trough
 */
const getMouseTarget = (
  mousePosition: Vec,
  cache: GeometryCache
): MouseTarget => {
  if (cache.nodes.size === 0) {
    return {
      _type: "nothing"
    }
  }

  const nodes = [...cache.nodes.values()]

  const distanceToMouse = (position: Vec) => dist(mousePosition, position)

  const distanceToMouseSq = (position: Vec) => distSq2(mousePosition, position)

  const closestOutput = minBy(
    (a, b) => {
      if (a === null || a.output === undefined) {
        return false
      }

      if (b === null || b.output === undefined) {
        return true
      }

      return distanceToMouseSq(a.output!.pos) < distanceToMouseSq(b.output!.pos)
    },
    nodes,
    null
  )

  if (
    closestOutput !== null &&
    distanceToMouse(closestOutput.output.pos) < pickDistance.output
  ) {
    return {
      _type: "nodeOutput",
      node: closestOutput
    }
  }

  const closestInput = minBy(
    (a, b) => {
      if (a === null) {
        return false
      }

      if (b === null) {
        return true
      }

      return distanceToMouse(a.closest) < distanceToMouse(b.closest)
    },
    nodes.flatMap((node) =>
      node.inputs[0].attribs?.selectable
        ? node.inputs.map((input, index) => ({
            index,
            node,
            closest: closestPoint(input, mousePosition)!
          }))
        : []
    ),
    null
  )

  if (
    closestInput &&
    distanceToMouse(closestInput.closest) < pickDistance.input
  ) {
    return {
      _type: "nodeInput",
      node: closestInput.node,
      index: closestInput.index,
      geom: closestInput.node.inputs[closestInput.index] as g.Arc
    }
  }

  return {
    _type: "nothing"
  }
}

/**
 * Handle a mouseMove event
 *
 * @param ctx The context to re-render to.
 */
export const onMouseMove = (ctx: CanvasRenderingContext2D) => (
  event: MouseEvent
) => (cache: GeometryCache) => () => {
  const mouse = [event.pageX, event.pageY]
  const transform = getMouseTransform(ctx)
  const mousePosition = mulV23(null, transform, mouse)

  const target = getMouseTarget(mousePosition, cache)
  const nodes = [...cache.nodes.values()]

  if (target._type === "nodeOutput" && cache.selectedOutput !== target.node) {
    if (cache.selectedOutput) {
      cache.selectedOutput.output.r = nodeOutputRadius.normal
    }

    cache.selectedOutput = target.node
    target.node!.output.r = nodeOutputRadius.onHover
  }

  if (target._type === "nodeInput" && cache.selectedInput !== target.geom) {
    if (cache.selectedInput) {
      cache.selectedInput.attribs!.weight = arcStrokeWidth.normal
    }

    cache.selectedInput = target.geom
    target.geom.attribs!.weight = arcStrokeWidth.onHover
  }

  // Clear old data from the cache
  if (target._type !== "nodeOutput" && cache.selectedOutput) {
    cache.selectedOutput.output.r = nodeOutputRadius.normal
    cache.selectedOutput = null
  }

  if (target._type !== "nodeInput" && cache.selectedInput) {
    cache.selectedInput.attribs!.weight = arcStrokeWidth.normal
    cache.selectedInput = null
  }

  renderScene(ctx, cache)
}

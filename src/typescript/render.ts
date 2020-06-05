import * as g from "@thi.ng/geom"
import { walk } from "@thi.ng/hdom-canvas"
import { Mat23Like, transform23 } from "@thi.ng/matrices"
import type {
  GeometryCache,
  NodeData,
  NodeId,
  NodeGeometry
} from "./types/Node"
import { Vec2Like } from "@thi.ng/vectors"
import * as Arc from "./arcs"
import {
  nodeRadius,
  inputLayerOffset,
  arcStrokeWidth,
  arcSpacing,
  constantInputStroke
} from "./constants"
import { TAU } from "@thi.ng/math"

// Used in the Default purescript implementation of GeomCache
export const emptyGeometryCache: GeometryCache = {
  nodes: new Map(),
  camera: transform23(null, [0, 0], 0, 1) as Mat23Like
}
/**
 * Create the geometry for a node input.
 *
 * @param position The position of the input
 */
export const dottedInput = (position: Vec2Like) => {
  const attribs = {
    stroke: constantInputStroke,
    weight: arcStrokeWidth,
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
    weight: arcStrokeWidth
  }

  const spacing = input.isCircle ? 0 : arcSpacing
  const radius = nodeRadius + step * inputLayerOffset

  if (input.isCircle) {
    return g.circle(position, radius, attribs)
  }

  return g.pathFromCubics(
    g.cubicFromArc(
      g.arc(
        position,
        radius,
        0,
        input.arc[0] + spacing,
        input.arc[1] - spacing + TAU * Number(input.arc[1] < input.arc[0])
      )
    ),
    attribs
  )
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

  return { inputs: inputGeom, output }
}

export const renderScene = (
  ctx: CanvasRenderingContext2D,
  cache: GeometryCache
) => {
  ctx.resetTransform()
  ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)

  const matrix = transform23(
    null,
    [ctx.canvas.width / 2, ctx.canvas.height / 2],
    0,
    1
  )

  const nodes = [...cache.nodes.values()].flatMap(({ inputs, output }) => [
    ...inputs,
    output
  ])

  const shapes = g.group({ transform: matrix }, nodes).toHiccup()

  walk(ctx, [shapes], {
    attribs: {},
    edits: []
  })
}

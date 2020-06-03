import { arcStrokeWidth, inputLayerOffset, nodeRadius } from "./constants"
import * as g from "@thi.ng/geom"
import { Vec2Like } from "@thi.ng/vectors"
import { walk } from "@thi.ng/hdom-canvas"
import { IHiccupShape } from "@thi.ng/geom-api"
import { transform23 } from "@thi.ng/matrices"

export interface InputData {
  output: string | null
  color: string
  arc: Vec2Like
  value: NodeId | null
}

export interface NodeData {
  position: [number, number]
  inputs: InputData[][]
}

export type NodeId = { readonly brand: unique symbol } & string

export type Effect<T> = () => T

export type GeometryCache = Map<NodeId, IHiccupShape>

export const renderInput = (
  position: Vec2Like,
  step: number,
  input: InputData
) => {
  return g.withAttribs(
    g.arc(
      position,
      step * nodeRadius + inputLayerOffset,
      0,
      input.arc[0],
      input.arc[1]
    ),
    { stroke: input.color, strokeWidth: arcStrokeWidth }
  )
}

export const renderNode = (node: NodeData) => {
  return g.circle(node.position, 10, { fill: "yellow" })
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

  const shapes = g.group({ transform: matrix }, [...cache.values()]).toHiccup()

  ctx.fillRect(0, 0, 100, 100)

  walk(ctx, [shapes], {
    attribs: {},
    edits: []
  })
}

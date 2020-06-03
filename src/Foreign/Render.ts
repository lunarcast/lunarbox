import * as Native from "src/typescript/render"
import { walk } from "@thi.ng/hdom-canvas"
import * as g from "@thi.ng/geom"
import { IShape, IHiccupShape } from "@thi.ng/geom-api"

export type NodeId = { readonly brand: unique symbol } & string

export type Effect<T> = () => T

export type GeometryCache = Map<NodeId, IHiccupShape>

export const emptyGeometryCache = new Map()

export const loadNode = (cache: GeometryCache) => (id: NodeId) => (
  data: Native.NodeData
) => () => {
  const shape = Native.renderNode(data)

  cache.set(id, shape)
}

/**
 * Render  a scene from purescript
 *
 * @param ctx The context to render to
 */
export const renderScene = (ctx: CanvasRenderingContext2D) => (
  cache: GeometryCache
) => () => {
  ctx.resetTransform()
  ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)

  const shapes = [...cache.values()]

  ctx.translate(100, 100)

  console.log(shapes)

  walk(ctx, shapes, {
    attribs: {},
    edits: []
  })
}

// To be able to get contexts from purescript
export const getContext = (canvas: HTMLCanvasElement) => () =>
  canvas.getContext("2d")

// Scale a canvas to its bounding box
export const resizeCanvas = (canvas: HTMLCanvasElement) => () => {
  const { width, height } = canvas.getBoundingClientRect()

  canvas.width = width
  canvas.height = height
}

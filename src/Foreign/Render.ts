import * as Native from "src/typescript/render"

// The initial cache used from purescript
export const emptyGeometryCache = new Map()

// Add a new node to the scene
export const loadNode = (cache: Native.GeometryCache) => (
  id: Native.NodeId
) => (data: Native.NodeData) => () => {
  const shape = Native.renderNode(data)

  cache.set(id, shape)
}

/**
 * Render  a scene from purescript
 *
 * @param ctx The context to render to
 */
export const renderScene = (ctx: CanvasRenderingContext2D) => (
  cache: Native.GeometryCache
) => () => Native.renderScene(ctx, cache)

// To be able to get contexts from purescript
export const getContext = (canvas: HTMLCanvasElement) => () =>
  canvas.getContext("2d")

// Scale a canvas to its bounding box
export const resizeCanvas = (canvas: HTMLCanvasElement) => () => {
  const { width, height } = canvas.getBoundingClientRect()

  canvas.width = width
  canvas.height = height
}

// Same as the above thing but works with rendering contexts instead
export const resizeContext = (ctx: CanvasRenderingContext2D) =>
  resizeCanvas(ctx.canvas)

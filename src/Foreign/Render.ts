import * as Native from "src/typescript/render"
import { GeometryCache } from "src/typescript/types/Node"

// The initial cache used from purescript
export const emptyGeometryCache = Native.emptyGeometryCache

/**
 * Render  a scene from purescript
 *
 * @param ctx The context to render to
 */
export const renderScene = (ctx: CanvasRenderingContext2D) => (
  cache: GeometryCache
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

// Reexports with a few changed names
export const handleMouseMove = Native.onMouseMove
export const handleMouseUp = Native.onMouseUp
export const handleMouseDown = Native.onMouseDown

// We cannot do
// export * from "../typescript/save"
// because purescript cannot understand it yet
import * as Save from "src/typescript/save"
import * as Sync from "src/typescript/sync"
export const geometryCacheToJson = Save.geometryCacheToJson
export const geometryCacheFromJsonImpl = Save.geometryCacheFromJson
export const createNode = Sync.createNode
export const refreshInputArcs = Sync.refreshInputArcs

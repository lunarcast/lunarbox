"use strict"

const Native = require("src/typescript/render.ts")

// The initial cache used from purescript
exports.emptyGeometryCache = Native.emptyGeometryCache

/**
 * Render  a scene from purescript
 *
 * @param ctx The context to render to
 */
exports.renderScene = (ctx) => (cache) => () => Native.renderScene(ctx, cache)

// To be able to get contexts from purescript
exports.getContext = (canvas) => () => canvas.getContext("2d")

// Scale a canvas to its bounding box
exports.resizeCanvas = (canvas) => () => {
  const { width, height } = canvas.getBoundingClientRect()

  canvas.width = width
  canvas.height = height
}

// Same as the above thing but works with rendering contexts instead
exports.resizeContext = (ctx) => exports.resizeCanvas(ctx.canvas)

// Reexports with a few changed names
exports.handleMouseMoveImpl = Native.onMouseMove
exports.handleMouseUpImpl = Native.onMouseUp
exports.handleMouseDownImpl = Native.onMouseDown

// We cannot do
// export * from "../typescript/save"
// because purescript cannot understand it yet
const Save = require("src/typescript/save.ts")
const Sync = require("src/typescript/sync.ts")
const Preview = require("src/typescript/preview.ts")

exports.geometryCacheToJson = Save.geometryCacheToJson
exports.geometryCacheFromJsonImpl = Save.geometryCacheFromJson
exports.createNode = Sync.createNode
exports.refreshInputArcs = Sync.refreshInputArcs
exports.setUnconnectableInputs = Sync.setUnconnectableInputs
exports.setUnconnectableOutputs = Sync.setUnconnectableOutputs
exports.deleteNode = Sync.deleteNode
exports.renderPreview = Preview.renderPreview

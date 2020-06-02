import { walk } from "@thi.ng/hdom-canvas"
import { renderScene } from "./render"
import { App } from "../Foreign/Render"

export const makeApp = (production: boolean): App => ({
  production,
  renderScene: (ctx) => (scene) => {
    return () => {
      ctx.resetTransform()
      ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)

      const shape = renderScene(scene)

      walk(ctx, [shape], {
        attribs: [],
        edits: []
      })
    }
  }
})

import { NodeState } from "./types/Node"
import { CanvasElement } from "./types/Hiccup"
import { nodeOutputRadius, nodeRadius, arcStrokeWidth } from "./constants"
import { dottedInput } from "./render"
import { draw } from "@thi.ng/hiccup-canvas"

/**
 * Draw the preview of a node.
 *
 * @param ctx The ctx to draw to.
 * @param colorMap The colors to use for the node.
 */
export const renderPreview = (ctx: CanvasRenderingContext2D) => (
  colorMap: NodeState["colorMap"]
) => () => {
  const { width, height } = ctx.canvas

  const shapes: Array<unknown | CanvasElement> = []

  if (colorMap.output) {
    shapes.push(["circle", { fill: colorMap.output }, [0, 0], nodeOutputRadius])
  }

  if (colorMap.inputs.length) {
    const inputLength = (2 * Math.PI) / colorMap.inputs.length

    for (let index = 0; index < colorMap.inputs.length; index++) {
      const stroke = colorMap.inputs[index]
      const start = index * inputLength

      shapes.push([
        "arc",
        { weight: arcStrokeWidth.normal, stroke },
        [0, 0],
        nodeRadius,
        start,
        start + inputLength
      ])
    }
  } else {
    shapes.push(dottedInput([0, 0]).toHiccup())
  }

  ctx.save()
  ctx.translate(width / 2, height / 2)

  draw(ctx, shapes)

  ctx.restore()
}

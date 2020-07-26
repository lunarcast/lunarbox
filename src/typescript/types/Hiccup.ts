import { Vec2Like } from "@thi.ng/vectors"
import { IToHiccup } from "@thi.ng/api"

type TextAlign = "center"

export type TextAttribs = {
  stroke: string
  align: TextAlign
  scale: number
  baseline: "hanging" | "baseline" | "middle"
  font: string
  fill: string

  // Used to know if the text changed so we can resize the background
  __dirtyBackground: boolean
}

export type TextElement = ["text", Partial<TextAttribs>, Vec2Like, string]

/**
 * Elements which can be rendered on a canvas.
 */
export type CanvasElement = TextElement | IToHiccup | null | CanvasElement[]

import { Vec2Like } from "@thi.ng/vectors"

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

import { Vec2Like } from "@thi.ng/vectors"

type TextAlign = "center"

type TextAttribs = {
  stroke: string
  align: TextAlign
  scale: number
  baseline: "hanging" | "baseline" | "middle"
  font: string
  fill: string
}

export type TextElement = ["text", Partial<TextAttribs>, Vec2Like, string]

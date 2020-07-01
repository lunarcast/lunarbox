import { Vec2Like } from "@thi.ng/vectors"

export type TextElement = [
  "text",
  Partial<{ stroke: string }>,
  Vec2Like,
  string
]

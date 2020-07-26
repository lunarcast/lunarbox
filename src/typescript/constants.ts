import { Vec2Like } from "@thi.ng/vectors"

export const inputLayerOffset = 10
export const nodeRadius = 50
export const arcSpacing = 0.1
export const constantInputStroke = `rgb(176, 112, 107)`
export const nodeBackgroundOpacity = 0.2

export const connectionWidth = {
  normal: 5,
  onHover: 10
}

export const textBgPadding = 10
export const nodeBackgrounds = {
  onHover: "#2483bf",
  selected: "#29adff"
}

export const arcStrokeWidth = {
  normal: 5,
  onHover: 10
}

export const nodeOutputRadius = {
  normal: 10,
  onHover: 15
}

export const pickDistance = {
  output: nodeOutputRadius.onHover,
  input: 10,
  node: nodeRadius
}

export const font = "normal normal bold 20px 'Roboto Mono', monospace"
export const textPadding: Vec2Like = [5, 5]

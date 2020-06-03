import { arcStrokeWidth, inputLayerOffset, nodeRadius } from "./constants"
import * as g from "@thi.ng/geom"
import { Vec2Like } from "@thi.ng/vectors"

type NodeId = string

export interface InputData {
  output: string | null
  color: string
  arc: Vec2Like
  value: NodeId | null
}

export interface NodeData {
  position: [number, number]
  inputs: InputData[][]
}

export interface SceneData {
  nodes: NodeData[]
}

export const renderInput = (
  position: Vec2Like,
  step: number,
  input: InputData
) => {
  return g.withAttribs(
    g.arc(
      position,
      step * nodeRadius + inputLayerOffset,
      0,
      input.arc[0],
      input.arc[1]
    ),
    { stroke: input.color, strokeWidth: arcStrokeWidth }
  )
}

export const renderNode = (node: NodeData) => {
  return g.group(
    {},
    node.inputs.flatMap((layer, index) =>
      layer.map((input) => renderInput(node.position, index, input))
    )
  )
}

export const renderScene = (scene: SceneData) => {
  return g.group({}, scene.nodes.map(renderNode))
}

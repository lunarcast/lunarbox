import { GeometryCache, NodeId, NodeState } from "./types/Node"
import * as Native from "./render"
import * as Arc from "./arcs"
import { Vec2Like } from "@thi.ng/vectors"
import { inputLayerOffset, nodeRadius } from "./constants"
import * as g from "@thi.ng/geom"
import { TAU } from "@thi.ng/math"

/**
 * Generate the geometries for a new node.
 *
 * @param cache The cache to mutate
 * @param id The id of the new node
 * @param inputCount The number of inputs arcs to generate for the new node
 */
export const createNode = (cache: GeometryCache) => (id: NodeId) => (
  inputCount: number
) => () => {
  const shape = Native.createNodeGeometry(
    [0, 0], // TODO: find a better way to place nodes
    inputCount
  )

  cache.zOrder.push(id)
  cache.nodes.set(id, shape)
}

/**
 * Takes some input data and moves the arcs of a node around based on that data.
 *
 * @param cache The cache to mutate
 * @param id The id of the node to refresh the arcs of
 * @param state The actual state to apply
 */
export const refreshInputArcs = (cache: GeometryCache) => (id: NodeId) => ({
  inputs,
  colorMap
}: NodeState) => () => {
  const node = cache.nodes.get(id)

  if (!node || !node.inputs[0].attribs!.selectable) {
    return
  }

  const arcs = Arc.placeInputs(
    (id) => cache.nodes.get(id)!.position as Vec2Like,
    inputs.map((output, index) => ({ output, color: colorMap.inputs[index] })),
    node.position as Vec2Like
  )

  if (node.output && colorMap.output) {
    node.output.attribs!.fill = colorMap.output
  }

  for (let i = 0; i < arcs.length; i++) {
    for (let j = 0; j < arcs[i].length; j++) {
      const arc = arcs[i][j]

      const geom = node.inputs[i * arcs.length + j] as g.Arc
      const radius = i * inputLayerOffset + nodeRadius

      geom.r = [radius, radius]
      geom.attribs!.fill = arc.color

      if (arc.isCircle) {
        geom.start = 0
        geom.end = TAU
      } else {
        geom.start = arc.arc[0]
        geom.end = arc.arc[1]
      }
    }
  }
}

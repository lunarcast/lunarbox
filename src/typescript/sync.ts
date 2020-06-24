import type { GeometryCache, NodeId, NodeState } from "./types/Node"
import * as Native from "./render"
import * as Arc from "./arcs"
import type { Vec2Like } from "@thi.ng/vectors"
import { inputLayerOffset, nodeRadius, arcSpacing } from "./constants"
import * as g from "@thi.ng/geom"
import { TAU } from "@thi.ng/math"

/**
 * Generate the geometries for a new node.
 *
 * @param cache The cache to mutate.
 * @param id The id of the new node.
 * @param inputCount The number of inputs arcs to generate for the new node.
 * @param hasOutput If this is true an output geometry will be generated as well.
 */
export const createNode = (cache: GeometryCache) => (id: NodeId) => (
  inputCount: number
) => (hasOutput: boolean) => () => {
  const shape = Native.createNodeGeometry(
    [0, 0], // TODO: find a better way to place nodes
    inputCount,
    hasOutput
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
 * @param positionOverwrites Useful for preview of connections and stuff like that.
 */
export const refreshInputArcsImpl = (
  cache: GeometryCache,
  id: NodeId,
  state: NodeState,
  positionOverwrites: Record<NodeId, Vec2Like> = {}
) => {
  const node = cache.nodes.get(id)

  if (!node) {
    return
  }

  const { colorMap, inputs } = state

  if (node.output !== null && colorMap.output) {
    node.output.attribs!.fill = colorMap.output
  }

  if (!node.inputs[0].attribs!.selectable) {
    return
  }

  node.lastState = state

  const arcs = Arc.placeInputs(
    (id) => positionOverwrites[id] ?? cache.nodes.get(id)?.position ?? [0, 0],
    inputs.map((output, index) => ({
      output: node.inputOverwrites[index] ?? output,
      color: colorMap.inputs[index],
      geom: node.inputs[index] as g.Arc
    })),
    node.position as Vec2Like
  )

  for (let i = 0; i < arcs.length; i++) {
    const layer = arcs[i]

    for (const arc of layer) {
      const geom = arc.geom
      const radius = i * inputLayerOffset + nodeRadius

      geom.r = [radius, radius]
      geom.attribs!.stroke = arc.color

      if (arc.isCircle) {
        geom.start = 0
        geom.end = TAU
      } else {
        geom.start = arc.arc[0] + arcSpacing
        geom.end = arc.arc[1] - arcSpacing + (arc.arc[1] < arc.arc[0] ? TAU : 0)
      }
    }
  }

  for (let index = 0; index < state.inputs.length; index++) {
    const connectedTo = state.inputs[index]
    const connection = node.connections[index]

    if (connectedTo === null) {
      connection.attribs!.connected = false
      continue
    }

    const input = node.inputs[index]
    const start = cache.nodes.get(connectedTo)!.position

    // TODO: handle the rare cases when input color != output color
    connection.attribs!.connected = true
    connection.attribs!.stroke = input.attribs!.stroke

    connection.points[0] = start
    connection.points[1] = g.closestPoint(input, start)!
  }
}

/**
 * Curried version for use from purescript.
 */
export const refreshInputArcs = (cache: GeometryCache) => (id: NodeId) => (
  state: NodeState
) => () => refreshInputArcsImpl(cache, id, state)

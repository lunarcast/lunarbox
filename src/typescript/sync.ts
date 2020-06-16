import { GeometryCache, NodeId } from "./types/Node"
import * as Native from "./render"

// Add a new node to the scene
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

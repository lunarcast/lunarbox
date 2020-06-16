import { GeometryCache, NodeId, NodeData } from "./types/Node"
import { Vec2Like } from "@thi.ng/vectors"
import * as Native from "./render"

// Add a new node to the scene
export const loadNode = (cache: GeometryCache) => (id: NodeId) => (
  data: NodeData
) => () => {
  const shape = Native.renderNode(
    (id) => (cache.nodes.get(id)?.output.pos ?? [0, 0]) as Vec2Like,
    data
  )

  cache.zOrder.push(id)
  cache.nodes.set(id, shape)
}

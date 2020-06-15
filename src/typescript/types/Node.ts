import type { IHiccupShape, IShape } from "@thi.ng/geom-api"
import type { Mat23Like } from "@thi.ng/matrices"
import { Circle, Arc } from "@thi.ng/geom"
import { Vec } from "@thi.ng/vectors"

export interface InputData {
  color: string
  output: NodeId | null
}

export interface NodeData {
  position: [number, number]
  inputs: InputData[]
}

export type NodeId = { readonly brand: unique symbol } & string

export type Effect<T> = () => T

export interface NodeGeometry {
  background: Circle
  output: Circle
  inputs: (Circle | Arc)[]
  position: Vec
}

export type GeometryCache = {
  nodes: Map<NodeId, NodeGeometry>
  camera: Mat23Like
  selectedOutput: NodeGeometry | null
  selectedNode: NodeGeometry | null
  selectedInput: Arc | null
  selectedNodes: Set<NodeId>
}

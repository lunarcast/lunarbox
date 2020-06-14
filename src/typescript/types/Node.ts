import type { IHiccupShape, IShape } from "@thi.ng/geom-api"
import type { Mat23Like } from "@thi.ng/matrices"
import { Circle } from "@thi.ng/geom"

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
  output: Circle
  inputs: IHiccupShape[]
  selected: Boolean
}

export type GeometryCache = {
  nodes: Map<NodeId, NodeGeometry>
  camera: Mat23Like
  selectedOutput: NodeGeometry | null
}

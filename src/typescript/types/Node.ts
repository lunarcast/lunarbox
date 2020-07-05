import type { Mat23Like } from "@thi.ng/matrices"
import type { Circle, Arc, Line, Rect } from "@thi.ng/geom"
import type { Vec, Vec2 } from "@thi.ng/vectors"
import type { DCons } from "@thi.ng/dcons"
import type { MouseTarget } from "../target"
import type { ADT } from "ts-adt"
import { TextElement } from "./Hiccup"
import { TextWithBackground } from "../components/TextWithBackground"

/**
 * Interface for everything which keeps track of a node.
 */
export interface IHasNode<N = NodeGeometry> {
  node: N
  id: NodeId
}

/**
 * Data we need to get to be able to update the way a node looks.
 */
export interface NodeState {
  colorMap: {
    inputs: Array<string | null>
    output: string | null
  }
  value: string | null
  inputs: Array<NodeId | null>
}

/**
 * This is basically just a string with a "brand".
 *
 * The "brand" prop is there to circumvent
 * typescript' lack of nominal typing.
 */
export type NodeId = { readonly brand: unique symbol } & string

/**
 * This keeps track of all the display-related stuff of a node.
 * Only used on the typescript side.
 */
export interface NodeGeometry {
  background: Circle
  output: Circle | null
  inputs: (Circle | Arc)[]
  connections: Line[]
  position: Vec
  lastState: NodeState | null
  inputOverwrites: Record<number, NodeId>
  valueText: TextWithBackground
  name: TextWithBackground | null
}

/**
 * Nodes which we are sure have an output geometry
 */
export type NodeWithOutput = NodeGeometry & {
  output: NonNullable<NodeGeometry["output"]>
}

/**
 * Different tags for the PartialConnection adt.
 */
export const enum PartialKind {
  Nothing,
  Input,
  Output
}

/**
 * We take this as an argument to some stuff so I made a separate type for it.
 */
export type InputPartialConnection = IHasNode & {
  index: number
  geom: Arc
  unconnectable: Set<NodeId>
}

/**
 * Type for the current inputs we cannot connect to.
 */
export type UnconnectableInputs = Set<{ index: number; id: NodeId }>

/**
 * We can either have nothing selected or a kind of pin (input or output).
 */
export type PartialConnection = ADT<{
  [PartialKind.Input]: InputPartialConnection
  [PartialKind.Output]: IHasNode & {
    unconnectable: UnconnectableInputs
  }
  [PartialKind.Nothing]: {}
}>

/**
 * A geometry cache is just the state of the display.
 * We mutate this as much as we want since it's only being used directly
 * on the typescript side of things. All functions interacting with it
 * return Effects so we can call them safely from the purescript side.
 */
export type GeometryCache = {
  nodes: Map<NodeId, NodeGeometry>
  camera: Mat23Like
  // TODO: make this use a single prop
  selectedOutput: NodeWithOutput | null
  selectedNode: NodeGeometry | null
  selectedInput: Arc | null
  selectedConnection: Line | null
  selectedNodes: Set<NodeId>
  zOrder: DCons<NodeId>
  dragging: null | MouseTarget
  connection: PartialConnection
  connectionPreview: Line
}

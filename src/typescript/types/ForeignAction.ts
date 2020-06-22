import { Fn3, Fn2, Fn } from "@thi.ng/api"
import { NodeId } from "./Node"

// Those are here so we can do purescript interop properly
export type ForeignAction =
  | { readonly createConnection: unique symbol }
  | { readonly selectInput: unique symbol }
  | { readonly selectOutput: unique symbol }
  | { readonly nothing: unique symbol }

export interface ForeignActionConfig {
  createConnection: Fn3<NodeId, NodeId, number, ForeignAction>
  selectInput: Fn2<NodeId, number, ForeignAction>
  selectOutput: Fn<NodeId, ForeignAction>
  nothing: ForeignAction
}

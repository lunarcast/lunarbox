import { Fn3, Fn2, Fn } from "@thi.ng/api"
import { NodeId } from "./Node"

// Those are here so we can do purescript interop properly
export type ForeignAction = { readonly foreignAction: unique symbol }

export interface ForeignActionConfig {
  createConnection: Fn3<NodeId, NodeId, number, ForeignAction>
  selectInput: Fn2<NodeId, number, ForeignAction>
  selectOutput: Fn<NodeId, ForeignAction>
  deleteConnection: Fn2<NodeId, number, ForeignAction>
  goto: Fn<NodeId, ForeignAction>
  editNode: Fn<NodeId, ForeignAction>
  nothing: ForeignAction
}

import { Fn3 } from "@thi.ng/api"
import { NodeId } from "./Node"

// Those are here so we can do purescript interop properly
export type ForeignAction =
  | { readonly createConnection: unique symbol }
  | { readonly nothing: unique symbol }

export interface ForeignActionConfig {
  createConnection: Fn3<NodeId, NodeId, number, ForeignAction>
  nothing: ForeignAction
}

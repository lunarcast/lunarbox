import type {
  NodeWithOutput,
  GeometryCache,
  IHasNode,
  NodeId
} from "./types/Node"
import type { ADT } from "ts-adt"
import * as g from "@thi.ng/geom"
import { Vec, dist, distSq2 } from "@thi.ng/vectors"
import { minBy } from "./helpers/minBy"
import { pickDistance } from "./constants"
import { closestPoint, pointInside } from "@thi.ng/geom"
import { findLast } from "./helpers/findLast"

/**
 * Possible tags for the MouseTarget adt.
 */
export enum MouseTargetKind {
  Nothing,
  Node,
  NodeInput,
  NodeOutput,
  Connection
}

/**
 * The data we carry around to show what input the mouse is over.
 */
export interface InputSelection<T> extends IHasNode {
  index: number
  geom: T
}

/**
 * This adt represents whatever the mouse is over at any state.
 */
export type MouseTarget = ADT<{
  [MouseTargetKind.NodeInput]: InputSelection<g.Arc>
  [MouseTargetKind.Connection]: InputSelection<g.Line>
  [MouseTargetKind.Node]: IHasNode
  [MouseTargetKind.NodeOutput]: IHasNode<NodeWithOutput>
  [MouseTargetKind.Nothing]: {}
}>

/**
 * Finds the object in the scene the mouse is hovering over
 *
 * @param mousePosition The position the mouse is at
 * @param cache The geometry cache to search trough
 */
export const getMouseTarget = (
  mousePosition: Vec,
  cache: GeometryCache
): MouseTarget => {
  if (cache.nodes.size === 0) {
    return {
      _type: MouseTargetKind.Nothing
    }
  }

  const nodes = [...cache.nodes.entries()]

  const distanceToMouse = (position: Vec) => dist(mousePosition, position)

  const distanceToMouseSq = (position: Vec) => distSq2(mousePosition, position)

  {
    const closestOutput: [NodeId, NodeWithOutput] | null = minBy(
      ([, a], [, b]) => {
        return distanceToMouseSq(a.output.pos) < distanceToMouseSq(b.output.pos)
      },
      nodes.filter(
        (input): input is [NodeId, NodeWithOutput] => input[1].output !== null
      )
    )

    if (
      closestOutput !== null &&
      distanceToMouse(closestOutput[1].output.pos) < pickDistance.output
    ) {
      return {
        _type: MouseTargetKind.NodeOutput,
        node: closestOutput[1],
        id: closestOutput[0]
      }
    }
  }
  {
    const closestInput = minBy(
      (a, b) => {
        return distanceToMouse(a.closest) < distanceToMouse(b.closest)
      },
      nodes.flatMap(([id, node]) =>
        node.inputs[0].attribs?.selectable
          ? node.inputs.map((input, index) => ({
              index,
              node,
              id,
              closest: closestPoint(input, mousePosition)!,
              geom: input
            }))
          : []
      )
    )

    if (
      closestInput &&
      distanceToMouse(closestInput.closest) < pickDistance.input
    ) {
      return {
        _type: MouseTargetKind.NodeInput,
        node: closestInput.node,
        id: closestInput.id,
        index: closestInput.index,
        geom: closestInput.geom as g.Arc
      }
    }
  }
  {
    const closestConnection = minBy(
      (a, b) => distanceToMouse(a.closest) < distanceToMouse(b.closest),
      nodes.flatMap(([id, node]) =>
        node.inputs[0].attribs?.selectable
          ? node.connections
              .filter((connection) => connection.attribs!.connected)
              .map((connection, index) => ({
                index,
                node,
                id,
                closest: closestPoint(connection, mousePosition)!,
                geom: connection
              }))
          : []
      )
    )

    if (
      closestConnection &&
      distanceToMouse(closestConnection.closest) < pickDistance.input
    ) {
      return {
        _type: MouseTargetKind.Connection,
        node: closestConnection.node,
        id: closestConnection.id,
        index: closestConnection.index,
        geom: closestConnection.geom
      }
    }
  }
  {
    const closestNode = findLast(
      [...cache.zOrder].map((id) => [id, cache.nodes.get(id)!] as const),
      ([, a]) => pointInside(a.background, mousePosition)
    )

    if (closestNode) {
      return {
        _type: MouseTargetKind.Node,
        node: closestNode[1],
        id: closestNode[0]
      }
    }
  }

  return {
    _type: MouseTargetKind.Nothing
  }
}

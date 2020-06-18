import type { NodeId, GeometryCache, IHasNode } from "./types/Node"
import type { ADT } from "ts-adt"
import * as g from "@thi.ng/geom"
import { Vec, dist, distSq2 } from "@thi.ng/vectors"
import { minBy } from "./helpers/minBy"
import { pickDistance } from "./constants"
import { closestPoint } from "@thi.ng/geom"

/**
 * Possible tags for the MouseTarget adt.
 */
export enum MouseTargetKind {
  Nothing,
  Node,
  NodeInput,
  NodeOutput
}

/**
 * This adt represents whatever the mouse is over at any state.
 */
export type MouseTarget = ADT<{
  [MouseTargetKind.NodeInput]: IHasNode & {
    index: number
    geom: g.Arc
  }
  [MouseTargetKind.Node]: IHasNode
  [MouseTargetKind.NodeOutput]: IHasNode
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

  const closestOutput = minBy(([, a], [, b]) => {
    return distanceToMouseSq(a.output!.pos) < distanceToMouseSq(b.output!.pos)
  }, nodes)

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
            closest: closestPoint(input, mousePosition)!
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
      geom: closestInput.node.inputs[closestInput.index] as g.Arc
    }
  }

  // Rn this is the same as the output one but in the future nodes might not have outputs.
  const closestNode = minBy(
    ([, a], [, b]) => {
      return distanceToMouseSq(a.output!.pos) < distanceToMouseSq(b.output!.pos)
    },
    [...cache.nodes.entries()]
  )

  if (
    closestNode &&
    distanceToMouse(closestNode[1].output!.pos) < pickDistance.node
  ) {
    return {
      _type: MouseTargetKind.Node,
      node: closestNode[1],
      id: closestNode[0]
    }
  }

  return {
    _type: MouseTargetKind.Nothing
  }
}
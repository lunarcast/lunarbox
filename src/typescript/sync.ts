import {
  GeometryCache,
  NodeId,
  NodeState,
  UnconnectableInputs,
  PartialKind
} from "./types/Node"
import * as Native from "./render"
import * as Arc from "./arcs"
import {
  Vec2Like,
  sub2,
  cartesian2,
  mulS2,
  invert2,
  neg
} from "@thi.ng/vectors"
import { inputLayerOffset, nodeRadius, arcSpacing } from "./constants"
import * as g from "@thi.ng/geom"
import { TAU } from "@thi.ng/math"
import * as color from "@thi.ng/color"
import type { IHiccupShape } from "@thi.ng/geom-api"
import { translation23, Mat23Like } from "@thi.ng/matrices"

/**
 * Generate the geometries for a new node.
 *
 * @param cache The cache to mutate.
 * @param id The id of the new node.
 * @param inputCount The number of inputs arcs to generate for the new node.
 * @param hasOutput If this is true an output geometry will be generated as well.
 */
export const createNode = (cache: GeometryCache) => (id: NodeId) => (
  inputCount: number
) => (hasOutput: boolean) => (name: string | null) => () => {
  const shape = Native.createNodeGeometry(
    [0, 0], // TODO: find a better way to place nodes
    inputCount,
    hasOutput,
    name
  )

  cache.zOrder.push(id)
  cache.nodes.set(id, shape)
}

/**
 * Takes some input data and moves the arcs of a node around based on that data.
 *
 * @param cache The cache to mutate
 * @param id The id of the node to refresh the arcs of
 * @param state The actual state to apply
 * @param positionOverwrites Useful for preview of connections and stuff like that.
 */
export const refreshInputArcsImpl = (
  cache: GeometryCache,
  id: NodeId,
  state: NodeState,
  positionOverwrites: Record<NodeId, Vec2Like> = {}
) => {
  const node = cache.nodes.get(id)

  if (!node) {
    return
  }

  const { colorMap, inputs, value } = state

  if (node.output !== null && colorMap.output) {
    node.output.attribs!.fill = colorMap.output

    if (node.output.attribs!.oldColor !== null) {
      shadeOut(node.output)
    }
  }

  node.lastState = state

  let offset = 0

  if (node.inputs[0].attribs!.selectable) {
    const arcs = Arc.placeInputs(
      (id) => positionOverwrites[id] ?? cache.nodes.get(id)?.position ?? [0, 0],
      inputs.map((output, index) => ({
        output: node.inputOverwrites[index] ?? output,
        color: colorMap.inputs[index],
        geom: node.inputs[index] as g.Arc
      })),
      node.position as Vec2Like
    )

    for (let i = 0; i < arcs.length; i++) {
      const layer = arcs[i]

      for (const arc of layer) {
        const geom = arc.geom
        const radius = i * inputLayerOffset + nodeRadius

        geom.r = [radius, radius]

        geom.attribs!.stroke = arc.color

        if (geom.attribs!.oldColor !== null) {
          shadeOut(geom, "stroke")
        }

        if (arc.isCircle) {
          geom.start = 0
          geom.end = TAU
        } else {
          geom.start = arc.arc[0] + arcSpacing
          geom.end =
            arc.arc[1] - arcSpacing + (arc.arc[1] < arc.arc[0] ? TAU : 0)
        }
      }
    }

    for (let index = 0; index < state.inputs.length; index++) {
      const connectedTo = state.inputs[index]
      const connection = node.connections[index]

      if (connectedTo === null) {
        connection.attribs!.connected = false
        continue
      }

      const input = node.inputs[index] as g.Arc
      const startNode = cache.nodes.get(connectedTo)!
      const start = startNode.position

      // TODO: handle the rare cases when input color != output color
      connection.attribs!.connected = true
      connection.attribs!.stroke = input.attribs!.stroke

      connection.points[0] = sub2(
        [],
        start,
        cartesian2(
          [],
          [(input.start + input.end) / 2, startNode.output!.r].reverse()
        )
      )
      connection.points[1] = g.closestPoint(input, start)!
    }

    offset = nodeRadius + arcs.length * inputLayerOffset
  } else offset = nodeRadius + inputLayerOffset

  node.valueText.pos[1] = node.position[1] + offset

  if (node.name) {
    node.name.pos[1] = node.position[1] - offset
    node.name.refresh()
  }

  node.valueText.value = value ?? ""

  // This only works because of the assumption a node needs to
  // either have an output or at least an input
  node.valueText.attribs.fill =
    node.output?.attribs!.fill ?? node.inputs[0].attribs!.stroke

  node.valueText.refresh()
}

/**
 * Curried version for use from purescript.
 */
export const refreshInputArcs = (cache: GeometryCache) => (id: NodeId) => (
  state: NodeState
) => () => refreshInputArcsImpl(cache, id, state)

/**
 * Reduce the opacity of a specific attribute on a shape
 * and save it in the oldColor attribute.
 *
 * @param shape The shape to fade out a little bit.
 * @param attr The name of the attribute to shade out.
 * @param alpha The amount to do it to.
 */
const shadeOut = (shape: IHiccupShape, attr = "fill", alpha = 0.3) => {
  const oldColor = shape.attribs![attr]

  shape.attribs!.oldColor = oldColor
  shape.attribs![attr] = color.setAlpha([], color.parseCss(oldColor), alpha)
}

/**
 * Update the list of inputs we cannot currently connect to.
 *
 * @param cache The cache to mutate.
 * @param unconnectable The set of unconnectable inputs.
 */
export const setUnconnectableInputs = (cache: GeometryCache) => (
  unconnectable: UnconnectableInputs
) => () => {
  if (cache.connection._type === PartialKind.Output) {
    cache.connection.unconnectable = unconnectable

    for (const { id, index } of unconnectable) {
      const input = cache.nodes.get(id)!.inputs[index]

      shadeOut(input, "stroke")
    }
  }
}

/**
 * Update the list of outputs we cannot currently connect to.
 *
 * @param cache The cache to mutate.
 * @param unconnectable The set of unconnectable inputs.
 */
export const setUnconnectableOutputs = (cache: GeometryCache) => (
  unconnectable: Set<NodeId>
) => () => {
  if (cache.connection._type === PartialKind.Input) {
    cache.connection.unconnectable = unconnectable

    for (const id of unconnectable) {
      const node = cache.nodes.get(id)!

      if (!node.output) continue

      shadeOut(node.output)
    }
  }
}

/**
 * Delete a node from a cache.
 *
 * @param cache The cache to mutate.
 * @param id The id of the node to delete.
 */
export const deleteNode = (cache: GeometryCache) => (id: NodeId) => () => {
  const node = cache.nodes.get(id)!
  cache.nodes.delete(id)

  const cell = cache.zOrder.find(id)

  if (cell) {
    cache.zOrder.remove(cell)
  }

  if (cache.selectedInput?.pos === node.position) {
    cache.selectedInput = null
  }

  if (cache.selectedNode === node) {
    cache.selectedNode = null
  }

  if (cache.selectedOutput === node.output) {
    cache.selectedOutput = null
  }

  cache.selectedConnection = null
}

/**
 * Move the camera over a particular node.
 *
 * @param cache The cache to mutate.
 * @param id The id of the node to focus on.
 */
export const centerNode = (cache: GeometryCache) => (id: NodeId) => () => {
  cache.camera = translation23(
    [],
    neg([], cache.nodes.get(id)!.position)
  ) as Mat23Like
}

/**
 * Same as centerNode but instead of an id it just focuses on the output node.
 *
 * @param cache The cache to mutate.
 */
export const centerOutput = (cache: GeometryCache) => () => {
  for (const [id, node] of cache.nodes.entries()) {
    if (node.output) continue

    centerNode(cache)(id)()
    return
  }
}

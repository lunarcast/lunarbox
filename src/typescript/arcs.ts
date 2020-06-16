import { Vec2Like, sub2, IVector } from "@thi.ng/vectors"
import { TAU, HALF_PI } from "@thi.ng/math"
import { NodeId, InputData } from "./types/Node"

export interface IArc {
  arc: Vec2Like
  isCircle?: boolean
}

/**
 * Input data implementing IArc
 */
export type InputWithArc = InputData & IArc

const normalizeAngle = (angle: number) =>
  (angle < 0.0 ? TAU - (-angle % TAU) : angle) % TAU

const intersect_ = (b: number, as: number, ae: number): boolean =>
  (as > ae && (b >= as || b <= ae)) || (b >= as && b <= ae)

/**
 * Calculate the length of an arc
 *
 * @param param0 The arc to measure
 */
export const length = <T extends IArc>({ arc: [start, end] }: T): number => {
  const delta = end - start
  return end > start ? delta : TAU + delta
}

// Normalize angles bigger than 2 pi
const normalize = <T extends IArc>(arc: T): T => {
  const {
    arc: [start, end]
  } = arc
  return {
    ...arc,
    arc: [normalizeAngle(start), normalizeAngle(end)]
  }
}

/**
 * Rotate an arc by a number of radians
 *
 * @param amount The number of radians to rotate the arc by
 * @param arc The arc to rotate
 */
const rotate = <T extends IArc>(amount: number, arc: T): T => {
  const {
    arc: [start, end]
  } = arc
  return normalize({ ...arc, arc: [start + amount, end + amount] })
}

/**
 * Check if 2 arcs intersect each other
 *
 * @param param0 The first arc
 * @param param1 THe second arc
 */
const intersect = <T extends IArc>(
  { arc: [s1, e1] }: T,
  { arc: [s2, e2] }: T
) =>
  intersect_(s2, s1, e1) ||
  intersect_(e2, s1, e1) ||
  intersect_(s1, s2, e2) ||
  intersect_(e1, s2, e2)

/**  Construct a full circle containing an arbitrary value */
const full = <T>(inner: T): T & IArc => ({
  ...inner,
  isCircle: true,
  arc: [0.0, TAU - 0.00001]
})

/**
 * Collects all the overlaps between arcs in an array
 *
 * @param arcs The array to search in
 */
const collectIntersections = <T extends IArc>(arcs: T[]) => {
  const result: Set<T> = new Set()

  for (const arc1 of arcs) {
    for (const arc2 of arcs) {
      if (arc1 !== arc2 && intersect(arc1, arc2)) {
        result.add(arc1).add(arc2)
      }
    }
  }

  return [...result]
}

/**
 * Moves arcs from a layer to another to prevent overlaps on the first one.
 *
 * @param arcs The arcs to separate the intersections of
 */
const moveIntersections = <T extends IArc>(
  arcs: T[]
): { newLayer: T[]; oldLayer: T[] } => {
  const intersections = collectIntersections(arcs)

  if (intersections.length) {
    const moved = moveIntersections(intersections.slice(1))
    return {
      ...moved,
      newLayer: [intersections[0], ...moved.newLayer]
    }
  } else {
    return { newLayer: [], oldLayer: arcs }
  }
}

/**
 * Solve all the overlaps between some arcs
 *
 * @param arcs The arcs to solve.
 */
const solveOverlaps = <T extends IArc>(arcs: T[]): T[][] => {
  const { newLayer, oldLayer } = moveIntersections(arcs)

  if (newLayer.length === 0) {
    return [oldLayer]
  }

  const next = solveOverlaps(newLayer)

  return [...next, oldLayer]
}

/**
 * Finds the arc closes to the start of another one
 *
 * @param arcs The arcs to search trough
 * @param target The arc to find the closest thing to
 */
const closestArc = <T extends IArc>(arcs: T[], target: T): T | null => {
  const deltas = arcs.flatMap((arc) => {
    if (arc === target) {
      return []
    }

    const delta = arc.arc[0] - target.arc[0]

    return [{ arc, delta: delta > 0 ? delta : TAU + delta }]
  })

  if (deltas.length === 0) {
    return null
  }

  return deltas.reduce((old, current) => {
    if (old.delta < current.delta) {
      return current
    }

    return old
  }, deltas[0]).arc
}

/**
 * Returns all the empty spaces between a bunch of arcs. assumes no overlaps.
 *
 * @param arcs The arcs to return the empty spaces between
 */
const emptySpaces = <T extends IArc>(arcs: T[]): IArc[] => {
  if (arcs.length === 0) {
    return [full({})]
  }

  if (arcs.length === 1) {
    if (arcs[0].isCircle) {
      return []
    }

    return [
      {
        arc: arcs[0].arc.reverse() as Vec2Like
      }
    ]
  }

  return arcs.flatMap((arc) => {
    const closest = closestArc(arcs, arc)

    if (closest === null) {
      return []
    }

    return [
      {
        arc: [arc.arc[1], closest.arc[0]]
      }
    ]
  })
}

/**
 * Split an array in multiple arrays of a chunk
 *
 * @param size THe sie of each chunk
 * @param array The array to split into chunks.
 */
const chunkArray = <T>(size: number, array: T[]): T[][] => {
  const result: T[][] = []

  let remaining = array.length

  for (let resultIndex = 0; remaining; resultIndex++) {
    result.push([])

    for (let index = 0; remaining && index < size; index++, remaining--) {
      result[resultIndex][index] = array[resultIndex * size + index]
    }
  }

  return result
}

const fillWith = <T>(data: T[], arcs: Array<T & IArc>): Array<T & IArc> => {
  const spaces = emptySpaces(arcs).map(normalize)
  const chunkSize = Math.ceil(data.length / spaces.length)

  const chunked = chunkArray(chunkSize, data)

  const zipped = chunked
    .map((value, index) => [spaces[index], value] as [IArc, T[]])
    .filter((value) => value[0] !== undefined)

  const filled = zipped.flatMap(([arc, keys]) => {
    const arcLength = length(arc) / keys.length

    return keys.map((data, index): T & IArc => {
      const start = arc.arc[0] + index * arcLength

      return {
        ...data,
        isCircle: arc.isCircle && keys.length === 1,
        arc: [start, start + arcLength]
      }
    })
  })

  return [...filled, ...arcs]
}

/**
 * Calculates the center angle of an arc
 *
 * @param arc The arc return the center of.
 */
export const center = <T extends IArc>(arc: T) =>
  normalizeAngle(arc.arc[0] + length(arc) / 2)

/**
 * Generate a good looking arc placement for the inputs of an arc.
 *
 * @param getData Function to be able to get the position of any node.
 * @param inputs The inputs to place around.
 * @param position The position of the current node.
 */
export const placeInputs = (
  getData: (id: NodeId) => Vec2Like,
  inputs: InputData[],
  position: Vec2Like
): InputWithArc[][] => {
  const connected = inputs.filter(({ output }) => output !== null)
  const unconnected = inputs.filter(({ output }) => output === null)

  const maxHalfArcLength =
    Math.PI / (connected.length == 1 ? 2.0 : connected.length)

  const connectedOverlapping = connected.map(
    (input): InputWithArc => {
      const other = getData(input.output!)
      const relative = sub2(null, other, position)
      const angle = Math.atan2(-relative[0], relative[1])

      return {
        ...input,
        arc: [angle - maxHalfArcLength, angle + maxHalfArcLength]
      }
    }
  )

  let connectedLayered = solveOverlaps(connectedOverlapping)

  if (unconnected.length) {
    const index = connectedLayered.length - 1
    connectedLayered[index] = fillWith(unconnected, connectedLayered[index])
  }

  return connectedLayered.map((arr) => arr.map((arc) => rotate(HALF_PI, arc)))
}

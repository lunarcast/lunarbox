import type { GeometryCache, NodeId } from "./types/Node"
import type { Mat23Like } from "@thi.ng/matrices"
import type { Vec2Like } from "@thi.ng/vectors"
import { emptyGeometryCache, createNodeGeometry } from "./render"
import { DCons } from "@thi.ng/dcons"

// The following section is for stuff related to saving / loading caches from / to json
interface SavedData {
  camera: Mat23Like
  nodes: Array<{
    id: NodeId
    position: Vec2Like
    hasOutput: boolean
    inputCount: number
    name?: string
  }>
}

// Those are here so we can do purescript interop properly
type Either<E, A> =
  | (E & { readonly left: unique symbol })
  | (A & { readonly right: unique symbol })

interface EitherConfig<E, A> {
  left: (err: E) => Either<E, A>
  right: (val: A) => Either<E, A>
}

/**
 * THe opposite of geometryCacheToJson
 *
 * @param json The json to generate the cache from
 */
export const geometryCacheFromJson = (
  config: EitherConfig<String, GeometryCache>
) => ({ camera, nodes }: SavedData): Either<String, GeometryCache> => {
  try {
    return config.right({
      ...emptyGeometryCache(),
      selectedNodes: new Set(),
      camera: camera,
      zOrder: new DCons(nodes.map(({ id }) => id)),
      nodes: new Map(
        nodes.map(({ id, position, inputCount, hasOutput, name }) => [
          id,
          createNodeGeometry(
            position,
            inputCount,
            // @ts-ignore I use a number as a boolean here which is ok in this context
            hasOutput,
            name
          )
        ])
      )
    })
  } catch (err) {
    return config.right(err.message)
  }
}

/**
 * Encode a geometry cache as json
 *
 * @param cache The cache to generate json from
 */
export const geometryCacheToJson = (cache: GeometryCache): SavedData => {
  const saved: SavedData = {
    camera: cache.camera,
    nodes: [...cache.zOrder].map((id) => {
      const node = cache.nodes.get(id)!

      return {
        id,
        position: node.position as Vec2Like,
        inputCount: node.inputs[0].attribs!.selectable ? node.inputs.length : 0,
        hasOutput: node.output !== null,
        name: node.name === null ? undefined : node.name.value
      }
    })
  }

  return saved
}

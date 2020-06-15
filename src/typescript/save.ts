import { GeometryCache, NodeId } from "./types/Node"
import { emptyGeometryCache, renderNode } from "./render"
import { Mat23Like } from "@thi.ng/matrices"
import { Vec2Like } from "@thi.ng/vectors"

// The following section is for stuff related to saving / loading caches from / to json
interface SavedData {
  camera: Mat23Like
  nodes: Array<
    [
      NodeId,
      {
        position: Vec2Like
      }
    ]
  >
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
    // While rendering it's useful to be able to
    const hashed = new Map(nodes)
    const getPosition = (id: NodeId) =>
      hashed.get(id)?.position ?? ([0, 0] as Vec2Like)

    return config.right({
      ...emptyGeometryCache,
      camera: camera,
      nodes: new Map(
        nodes.map(([id, data]) => [
          id,
          renderNode(getPosition, { inputs: [], position: data.position })
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
    nodes: [...cache.nodes.entries()].map(([id, node]) => [
      id,
      {
        position: node.position as Vec2Like
      }
    ])
  }

  return saved
}

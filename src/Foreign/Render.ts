import { SceneData } from "../typescript/render"

export type Effect<T> = () => T

export interface App {
  production: boolean
  renderScene: (
    ctx: CanvasRenderingContext2D
  ) => (scene: SceneData) => Effect<void>
}

// To be able to get contexts from purescript
export const getContext = (canvas: HTMLCanvasElement) => () =>
  canvas.getContext("2d")

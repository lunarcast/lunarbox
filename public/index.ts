import "./styles/index.scss"
import { main as mainImpl } from "../output/Main"

type Effect<T> = (v: T) => void

const main = mainImpl as (prod: boolean) => (apiUrl: string) => Effect<void>

const production = process.env.NODE_ENV === "production"

if (production) {
  throw new Error("Api not deployed anywhere at the moment")
}
const apiUrl = "http://localhost:8090"

const start = main(production)(apiUrl)

if (!production && module.hot) {
  module.hot.accept(() => location.reload(true))
}

start()

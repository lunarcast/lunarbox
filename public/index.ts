import "./styles/index.scss"
import { main as mainImpl } from "../output/Main"

type Effect<T> = (v: T) => void

const main = mainImpl as (prod: boolean) => Effect<void>

const production = process.env.NODE_ENV === "production"
const start = main(production)

if (!production && module.hot) {
  module.hot.accept(() => location.reload(true))
}

start()

import "./styles/index.scss"
import { main as mainImpl } from "../output/Main"
import { Effect } from "../src/Foreign/Render"

const main = mainImpl as (prod: boolean) => Effect<void>

const production = process.env.NODE_ENV === "production"
const start = main(production)

if (!production && module.hot) {
  module.hot.accept(() => location.reload(true))
}

start()

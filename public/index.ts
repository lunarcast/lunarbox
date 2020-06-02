import "./styles/index.scss"
import { main as mainImpl } from "../output/Main"
import { makeApp } from "../src/typescript"
import { App, Effect } from "../src/Foreign/Render"

const main = mainImpl as (app: App) => Effect<void>

const production = process.env.NODE_ENV === "production"
const app = makeApp(production)
const start = main(app)

if (!production && module.hot) {
  module.hot.accept(() => location.reload(true))
}

start()

import "./styles/index.scss";
import { main } from "../output/Main";

const production = process.env.NODE_ENV === "production";
const start = main(production);

if (!production && module.hot) {
  module.hot.accept(() => location.reload(true));
}

start();

import "./styles/index.scss";
import "../output/bundle";

const production = process.env.NODE_ENV === "production";

if (!production && module.hot) {
  // module.hot.accept(() => location.reload(true));
  module.hot.accept(() => location.reload(true));
}

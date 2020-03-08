import Main from "../output/Main";
import "./styles/index.scss";

function main() {
  console.clear();
  Main.main();
}

if (module.hot) {
  module.hot.accept(function() {
    location.reload();
  });
}

main();

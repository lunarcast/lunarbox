import Main from "./output/Main";
import React from "react";
import { render } from "react-dom";

function main() {
  render(<Main.counter />, document.getElementById("app"));
}

if (module.hot) {
  module.hot.accept(function() {
    location.reload();
  });
}
console.clear();
main();

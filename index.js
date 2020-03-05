import Main from "./output/Main";

function main() {
  Main.main();
}

if (module.hot) {
  module.hot.accept(function() {
    location.reload();
  });
}

console.clear();
main();

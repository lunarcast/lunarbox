import Main from './output/Main';

function main() {
  // put environment variables here, pass them into Main.main
  Main.main();
}

if (module.hot) {
  module.hot.accept(function() {
    location.reload();
  });
}

console.log('starting');
main();

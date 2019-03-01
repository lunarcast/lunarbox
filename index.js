import Main from './output/Main';

function main() {
  Main.main();
}

if (module.hot) {
  module.hot.accept(function() {
    // console.log('running main again');
    // main();
  });
}

console.log('starting');
main();

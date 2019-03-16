import Main from './output/Main';

function main() {
  const body = document.querySelector('body');
  Array.from(body.children).forEach(element => {
    body.removeChild(element);
  });
  Main.main();
}

// if (module.hot) {
//   module.hot.accept(function() {
//     // console.log('running main again');
//     // main();
//   });
// }

console.log('starting');
main();

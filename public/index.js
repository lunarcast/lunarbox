import "./styles/index.scss";

const Main = import("../output/bundle");

if (process.env.NODE_ENV === "development" && module.hot) {
  // module.hot.accept(() => location.reload(true));
  module.hot.accept(() => console.log("that's hot"));
}

Main.then(({ main }) => {
  console.log("loaded purescript code");

  try {
    main();
  } catch (err) {
    console.err(`An error ocurred in the purescript code: ${err}`);
  }
});

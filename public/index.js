import "./styles/index.scss";

const Main = import("../output/bundle");

if (process.env.NODE_ENV === "development" && module.hot) {
  module.hot.accept(location.reload);
}

Main.then(({ main }) => {
  console.clear();
  console.log("loaded purescript code");

  try {
    main();
  } catch (err) {
    console.err(err);
  }
});

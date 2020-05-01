import "./styles/index.scss";

interface Entry {
  main: (production: boolean) => () => void;
}

const Main: Promise<Entry> = import("../output/bundle") as Promise<any>;
const production = process.env.NODE_ENV === "production";

if (!production && module.hot) {
  // module.hot.accept(() => location.reload(true));
  module.hot.accept(() => location.reload(true));
}

Main.then(({ main }) => {
  console.log("loaded purescript code");

  try {
    const start = main(production);

    start();
  } catch (err) {
    console.error(`An error ocurred in the purescript code: ${err}`);
  }
});

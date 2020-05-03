import "./styles/index.scss";

const production = process.env.NODE_ENV === "production";

if (production) {
  import("../output/prod-bundle");
} else {
  import("../output/Main").then(({ main }) => {
    console.log("Loaded purescript code");

    main();
  });

  if (module.hot) {
    module.hot.accept(() => location.reload(true));
  }
}

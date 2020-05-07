# Contributing

Fork this repo, write some code and submit a pull request into the develop branch.

## Installing locally

This guide assumes you have pnpm, purescript and spago alreay installed.

### Installing the dependencies

Clone this repo. Install the dependencies with:

```sh
pnpm install
```

Then you need to do the initial build for all the purescript and typescript stuff with:

```sh
spago build && pnpx tsc
```

> Note: this project uses [pnpm](https://pnpm.js.org), pull requests using npm or yarn will be ignored

### Running the dev server:

To start the dev server use the `dev` command:

```sh
pnpm run dev
```

> This project has a very small amount of typescript code, but if you plan to edit that be sure to also start tsc in watch mode with `pnpx tsc -w`

### Building for production

To generate a production build run:

```sh
pnpm run build
```

## Code generation

This project uses [hygen](http://www.hygen.io/) for code generation.

> If you want to contribute a new generator add it to the `_templates` folder

### Creating components / pages

To create a component you can run:

```
pnpx hygen purescript component Foo.Bar
```

This will create an empty halogen component in `src/Component/Foo/Bar.purs` which lives in a module called `Lunarbox.Component.Foo.Bar`.

> You can add the -p flag at the end to generate a page instead (lives in `src/Page` and has the module name prefixed with `Lunarbox.Page`)

> You can also use the render-function action when you want to build a render-function which takes some Input and some Actions as it's parameters

### Creating modules

To create a simple purescript module use can use the `module` action from the `purescript` generator:

```sh
hygen purescript module Foo.Bar
```

This will create an empty purescript module in `src/Foo/Bar.purs` called `Lunarbox.Foo.Bar` which has a single import to `Prelude`.

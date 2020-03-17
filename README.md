# Lunarbox

Tool to help begginers learn functional programming

## Contributing

Fork this repo, write some code and submit a pull request.

## Tehnologies:

This project is mostly made in the [puresciprt](https://www.purescript.org) programming language using [haolgen](https://github.com/purescript-halogen/purescript-halogen) for web rendering.

For installing dependencies this project uses:

- [pnpm](https://pnpm.js.org) for the js dependencies
- [spago](https://github.com/purescript/spago) for the purescript dependencies

For the compilation this project uses [parcel](https://parceljs.org) to bundle the js, [scss](https://sass-lang.com) and the assets and [spago](https://github.com/purescript/spago) to compile the purescript.

## Installing locally

### Installing the dependencies

Clone this repo. Install the dependencies with:

```sh
pnpm install
```

> Note: this project uses [pnpm](https://pnpm.js.org), pull requests using npm or yarn will be ignored

> Note: You also need to have purescript installed, in the future I might add purescript as a dev dependency, but I'm not sure if that would break anything.

### Running the dev server:

To start the dev server use the `dev` command:

```sh
pnpm run dev
```

> Note: the first build might take a long time, this is caused by the fact spago will have to install all it's dependencies

## Strucure & architecture

TODO

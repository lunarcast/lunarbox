# Lunarbox

<!-- ALL-CONTRIBUTORS-BADGE:START - Do not remove or modify this section -->
[![All Contributors](https://img.shields.io/badge/all_contributors-3-orange.svg?style=flat-square)](#contributors-)
<!-- ALL-CONTRIBUTORS-BADGE:END -->

![](https://img.shields.io/github/release-date/Mateiadrielrafael/lunarbox?label=Last%20release&style=for-the-badge) ![](https://img.shields.io/github/v/release/Mateiadrielrafael/lunarbox?style=for-the-badge) ![](https://img.shields.io/github/languages/top/Mateiadrielrafael/lunarbox?color=yellow&style=for-the-badge) ![](https://img.shields.io/github/workflow/status/Mateiadrielrafael/lunarbox/Test%20⛳/develop?style=for-the-badge)

[![forthebadge](https://forthebadge.com/images/badges/powered-by-water.svg)](https://forthebadge.com) [![forthebadge](https://forthebadge.com/images/badges/built-with-love.svg)](https://forthebadge.com)

Tool to help beginners learn functional programming

## Contributing

Fork this repo, write some code and submit a pull request.

## Tehnologies:

This project is mostly made in the [purescript](https://www.purescript.org) programming language using [halogen](https://github.com/purescript-halogen/purescript-halogen) for web rendering.

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

### Running the dev server:

To start the dev server use the `dev` command:

```sh
pnpm run dev
```

> Note: the first build might take a long time. This is caused by the fact spago will install and build all purescript dependencies

## Code generation

This project uses [hygen](http://www.hygen.io/) for code generation.

> If you want to contribute a new generator add it to the `_templates` folder

### Creating components / pages

To create a component you can run:

```
pnpx hygen purescript component Foo.Bar
```

This will create an empty halogen component in `src/Component/Foo/Bar.purs` which lives in a module called `Lunarbox.Component.Foo.Bar`.

> Note: you can add the -p flag at the end to generate a page instead (lives in `src/Page` and has the module name prefixed with `Lunarbox.Page`)

### Creating modules

To create a simple purescript module use can use the `module` action from the `purescript` generator:

```sh
hygen purescript module Foo.Bar
```

This will create an empty halogen component in `src/Foo/Bar.purs` which lives in a module called `Lunarbox.Foo.Bar` which has a single import to `Prelude`.

## Strucure & architecture

TODO

## Contributors ✨

Thanks goes to these wonderful people ([emoji key](https://allcontributors.org/docs/en/emoji-key)):

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tr>
    <td align="center"><a href="https://github.com/Mateiadrielrafael"><img src="https://avatars0.githubusercontent.com/u/39400800?v=4" width="100px;" alt=""/><br /><sub><b>Matei Adriel</b></sub></a><br /><a href="https://github.com/Mateiadrielrafael/lunarbox/commits?author=Mateiadrielrafael" title="Code">💻</a> <a href="#design-Mateiadrielrafael" title="Design">🎨</a> <a href="#infra-Mateiadrielrafael" title="Infrastructure (Hosting, Build-Tools, etc)">🚇</a></td>
    <td align="center"><a href="http://xwafl.github.io/portfolio"><img src="https://avatars2.githubusercontent.com/u/35458851?v=4" width="100px;" alt=""/><br /><sub><b>xWafl</b></sub></a><br /><a href="#design-xWafl" title="Design">🎨</a> <a href="#ideas-xWafl" title="Ideas, Planning, & Feedback">🤔</a> <a href="https://github.com/Mateiadrielrafael/lunarbox/commits?author=xWafl" title="Documentation">📖</a></td>
    <td align="center"><a href="https://discordapp.com/users/270972671490129921"><img src="https://avatars0.githubusercontent.com/u/49570123?v=4" width="100px;" alt=""/><br /><sub><b>Sandu Victor</b></sub></a><br /><a href="#design-Vyctor661" title="Design">🎨</a> <a href="#ideas-Vyctor661" title="Ideas, Planning, & Feedback">🤔</a></td>
  </tr>
</table>

<!-- markdownlint-enable -->
<!-- prettier-ignore-end -->
<!-- ALL-CONTRIBUTORS-LIST:END -->

This project follows the [all-contributors](https://github.com/all-contributors/all-contributors) specification. Contributions of any kind welcome!

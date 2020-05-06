# Lunarbox

<!-- ALL-CONTRIBUTORS-BADGE:START - Do not remove or modify this section -->

[![All Contributors](https://img.shields.io/badge/all_contributors-3-orange.svg?style=for-the-badge)](#contributors)

<!-- ALL-CONTRIBUTORS-BADGE:END -->

[![forthebadge](https://forthebadge.com/images/badges/built-with-love.svg)](https://forthebadge.com) [![forthebadge](https://forthebadge.com/images/badges/powered-by-water.svg)](https://forthebadge.com)

![](https://img.shields.io/github/release-date/Mateiadrielrafael/lunarbox?label=Last%20release&style=for-the-badge) ![](https://img.shields.io/github/v/release/Mateiadrielrafael/lunarbox?style=for-the-badge) ![](https://img.shields.io/github/languages/top/Mateiadrielrafael/lunarbox?color=yellow&style=for-the-badge) ![](https://img.shields.io/github/workflow/status/Mateiadrielrafael/lunarbox/Test%20⛳/develop?style=for-the-badge)

Tool to help beginners learn functional programming

## Documentation

You can find the infoeducatie documentation (written in romanian) [here](./infoeducatie/documentatie.md)

> Also for infoeducatie I also made a list with everything I used which I didn't make which you can find [here](./infoeducatie/external.md)

> Todo: usage guides

## Contributing

Fork this repo, write some code and submit a pull request.

## Installing locally

This guide assumes you have purescript and spago alreay installed.

### Installing the dependencies

Clone this repo. Install the dependencies with:

```sh
pnpm install
```

Then build all the purescript stuff with:

```sh
spago build
```

> Note: this project uses [pnpm](https://pnpm.js.org), pull requests using npm or yarn will be ignored

### Running the dev server:

To start the dev server use the `dev` command:

```sh
pnpm run dev
```

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

> Note: you can add the -p flag at the end to generate a page instead (lives in `src/Page` and has the module name prefixed with `Lunarbox.Page`)

> Note 2: you can also use the render-function action when you want to build a render-function which takes some Input and some Actions as it's parameters

### Creating modules

To create a simple purescript module use can use the `module` action from the `purescript` generator:

```sh
hygen purescript module Foo.Bar
```

This will create an empty halogen component in `src/Foo/Bar.purs` which lives in a module called `Lunarbox.Foo.Bar` which has a single import to `Prelude`.

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

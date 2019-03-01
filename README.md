## Pure HMR

This is a nifty little executable script that will quickly get a minimal hot reloading Purescript project going.

This runs the spago build system, as well as the parcel bundler. support for other build systems is in our roadmap.

This also assumes that your entry point is index.html. Flexible entry points are also in our roadmap

Props to justinwoo - https://github.com/justinwoo/purescript-parcel-example

### Setup:

1. Global dependancies

You must have the purescript compiler installed, as well as:

    npm i -g parcel spago

You must have inotifywatch installed - in linux:

    sudo apt install inotify-tools

2. Usage:

Copy the following files into your project directory.

./index.html
./index.js

If you're in an established project and you have an existing entry point, you may need to merge your existing code into these files, maintaining the import statement and script tag.

now run:

    ./purehmr

Happy Hacking!

### Roadmap Features

- allow the user to provide the build script as an argument

- allow the user to specify an entry point and watch folder.

"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
require("./styles/index.scss");
const Main_1 = require("../dce-output/Main");
const main = Main_1.main;
const production = process.env.NODE_ENV === "production";
const start = main(production);
if (!production && module.hot) {
    module.hot.accept(() => location.reload(true));
}
start();

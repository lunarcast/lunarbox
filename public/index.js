"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
require("./styles/index.scss");
const Main_1 = require("../output/Main");
const typescript_1 = require("../src/typescript");
const main = Main_1.main;
const production = process.env.NODE_ENV === "production";
const app = typescript_1.makeApp(production);
const start = main(app);
if (!production && module.hot) {
    module.hot.accept(() => location.reload(true));
}
start();

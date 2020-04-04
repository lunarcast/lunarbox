# [1.6.0](https://github.com/Mateiadrielrafael/lunarbox/compare/v1.5.0...v1.6.0) (2020-04-04)


### Features

* Chain expressions, and fixed all functions having the Null type ([ebfa5c9](https://github.com/Mateiadrielrafael/lunarbox/commit/ebfa5c9387d1551c49cde08506eb87106c438f87))
* the Nowhere location lol ([d05e5dc](https://github.com/Mateiadrielrafael/lunarbox/commit/d05e5dca5743cd7ffa1f5f964cc128bbc576f86e))

# [1.5.0](https://github.com/Mateiadrielrafael/lunarbox/compare/v1.4.1...v1.5.0) (2020-04-02)


### Bug Fixes

* fixed 50+ bugs caused by the addition of the location system ([6e86b30](https://github.com/Mateiadrielrafael/lunarbox/commit/6e86b30dcc41dc56988f25254597b61ae3d8c5bc))
* fixed all the remaining bugs caused by the refactoring ([bd05eb6](https://github.com/Mateiadrielrafael/lunarbox/commit/bd05eb610c08dec710569257294b28d3e1fea90a))
* fixed the Infer module not exporting runInfer ([696c1b5](https://github.com/Mateiadrielrafael/lunarbox/commit/696c1b540111557f1b169ae73a8fd55310855af4))
* now the current funciton appears in the tree view as well ([662a4a4](https://github.com/Mateiadrielrafael/lunarbox/commit/662a4a4acdef05ff7ab510388bd78781f7386662))
* now the Infer monad uses the new Constraint module ([74af345](https://github.com/Mateiadrielrafael/lunarbox/commit/74af34595e639d1cf1d287f88184311ac7677510))


### Features

* added the Unifier module and derived a bunch of eq instances ([0a6664c](https://github.com/Mateiadrielrafael/lunarbox/commit/0a6664cb94c68d261c28cca47f5ad7f575264b21))
* automatically infer the number of inputs for native expressions ([8d27f51](https://github.com/Mateiadrielrafael/lunarbox/commit/8d27f51d851639ac3e382d161994337faf217d92))
* created the Solve monad ([f4e5fb4](https://github.com/Mateiadrielrafael/lunarbox/commit/f4e5fb4898da04edd577c36685e98b38d7337897))
* displaying types for nodes above the nodes ([b58d4b0](https://github.com/Mateiadrielrafael/lunarbox/commit/b58d4b004aaa1275f20426bebd3d37c44253495c))
* finally combined the constraint solver with the type inference algorithm! ([05f836e](https://github.com/Mateiadrielrafael/lunarbox/commit/05f836eaedda98378bd58999cdf03787a4cd5476))
* lenses for the Infer monad related types ([dac26d6](https://github.com/Mateiadrielrafael/lunarbox/commit/dac26d6ea3401b6267804cf0f3d51ee01eeb7cb3))
* nativeConfigs can specify how much to curry, not visual yet ([084bfe5](https://github.com/Mateiadrielrafael/lunarbox/commit/084bfe5798945af1683ffb945bd508c1446ea5ec))
* now constraints keep a reference to the location they were emited from ([c8de391](https://github.com/Mateiadrielrafael/lunarbox/commit/c8de391a484aeaafb3b45b4494bc47eef1fbe153))
* only the code specific to each node is displayed on top of it ([63c9a70](https://github.com/Mateiadrielrafael/lunarbox/commit/63c9a7033fbcb61c2227aaf15255f77d8fee4589))
* reimplemented unification of types ([3eaba72](https://github.com/Mateiadrielrafael/lunarbox/commit/3eaba72918c5687b7784e10e8824250904b12015))
* Reworked a lot of stuff to allow locations! ([68952d3](https://github.com/Mateiadrielrafael/lunarbox/commit/68952d3ccd9498f246c640517d9e319897d22265))
* reworked some node ui ([617fdce](https://github.com/Mateiadrielrafael/lunarbox/commit/617fdce0f973e7f04d67b4fb4c3fbe54b3d201eb))
* reworked the expression inference algorithm ([eac02b3](https://github.com/Mateiadrielrafael/lunarbox/commit/eac02b31876d26724aa6346080417fb7a180eed9))
* running the Infer monad ([09cff84](https://github.com/Mateiadrielrafael/lunarbox/commit/09cff849bba21c84afb2ba8a8c188215ca2a2678))
* the expression of each node is displayed above it ([e156500](https://github.com/Mateiadrielrafael/lunarbox/commit/e156500e008dd0252070a3a52a5ccc82708b47f2))
* the Infer monad now support a location ([73b8148](https://github.com/Mateiadrielrafael/lunarbox/commit/73b81482552b4ec0e3d8d4838ecd412cc0c96fda))
* typeMaps!!! ([4f73f39](https://github.com/Mateiadrielrafael/lunarbox/commit/4f73f3904a97dc8319b89d31c50a81d940b286c9))
* working locations inside expressions:) FINALLY!!! ([3914234](https://github.com/Mateiadrielrafael/lunarbox/commit/391423455573da07a87f2ecaca6c221c690a7fda))
* wrote the constraint solver! ([caaef5d](https://github.com/Mateiadrielrafael/lunarbox/commit/caaef5dee1c154f35ad920e3bea4b7bcd357963d))

## [1.4.1](https://github.com/Mateiadrielrafael/lunarbox/compare/v1.4.0...v1.4.1) (2020-03-30)


### Bug Fixes

* adding new nodes now doesn't mess up the z-ordering anymore ([16b1941](https://github.com/Mateiadrielrafael/lunarbox/commit/16b19417ce6299e3a86a4bd11191369dada4769a))
* fixed selectable images and icons ([0bb78bb](https://github.com/Mateiadrielrafael/lunarbox/commit/0bb78bb00fb51280fbe133693c63a634b34d8c66))
* now theres no on-hover hilighting on non-usable icons ([59ff8be](https://github.com/Mateiadrielrafael/lunarbox/commit/59ff8bec7d79c66f0254344877a49eb1b5d115ff))
* now you cannot edit something you are already editing ([dafd108](https://github.com/Mateiadrielrafael/lunarbox/commit/dafd1089f8f57089b4f2cf5ce6accf67e07d0f75))

# [1.4.0](https://github.com/Mateiadrielrafael/lunarbox/compare/v1.3.0...v1.4.0) (2020-03-29)


### Bug Fixes

* now the native functions don't appear in the tree view anymore ([2096046](https://github.com/Mateiadrielrafael/lunarbox/commit/2096046a4118ceb9d8d5bd1f59921261f891a052))


### Features

* finally added a decent Native & Runtime system, and as a demo an add node is preloaded! ([a07fd62](https://github.com/Mateiadrielrafael/lunarbox/commit/a07fd6270e3ede20538b9033ab67f340dd499abd))
* given the Node component access the the FunctionData record ([596d26e](https://github.com/Mateiadrielrafael/lunarbox/commit/596d26e06cebbaaeb730225146f200cd43df9871))

# [1.3.0](https://github.com/Mateiadrielrafael/lunarbox/compare/v1.2.0...v1.3.0) (2020-03-28)


### Features

* the stuff on the add node panel now actually works! ([421e1b8](https://github.com/Mateiadrielrafael/lunarbox/commit/421e1b8549938f20a15ca4b170531f6127d06cc9))

# [1.2.0](https://github.com/Mateiadrielrafael/lunarbox/compare/v1.1.0...v1.2.0) (2020-03-25)


### Bug Fixes

* fixed everything related to switching to the new graph module ([4900c5f](https://github.com/Mateiadrielrafael/lunarbox/commit/4900c5fbd53aad4630c5055952910c5147979e2c))


### Features

* a basic Add page ([5e34b2c](https://github.com/Mateiadrielrafael/lunarbox/commit/5e34b2cac24f0c7624f72a6f0957295f66a45579))
* my own graph implementation so I can use lenses on it ([034fcdd](https://github.com/Mateiadrielrafael/lunarbox/commit/034fcddeb8c5e7c4eef638a73345fd591f4ac511))
* POC custom textures ([8b35099](https://github.com/Mateiadrielrafael/lunarbox/commit/8b350990dbc0d63dc7411dbc062bb033826aae01))
* the edit button on the Add page works now! ([2662986](https://github.com/Mateiadrielrafael/lunarbox/commit/26629862fe4a7bdde73bcd879cf49ceb0b7d061c))

# [1.1.0](https://github.com/Mateiadrielrafael/lunarbox/compare/v1.0.1...v1.1.0) (2020-03-23)


### Features

* moved everything related to not editing anything in the Editor component ([163f7fa](https://github.com/Mateiadrielrafael/lunarbox/commit/163f7fa8fdb749e3d577580417d1b71b4980e8ce))
* now the state of each function is saved when changing functions ([cc07c60](https://github.com/Mateiadrielrafael/lunarbox/commit/cc07c609d65c3f6886b18bcf13f51d3a3065e349))
* some more work on the node component ([c2e6420](https://github.com/Mateiadrielrafael/lunarbox/commit/c2e64203674f2716b837efb28d3ac76bbf935531))
* the node part of the dnd system ([19a8e40](https://github.com/Mateiadrielrafael/lunarbox/commit/19a8e40c9b606d2d9ac435dbc9123f9cf01b7321))
* the nodes are now unselected when you mouseup, bet you didn't expect that:) ([306d6d6](https://github.com/Mateiadrielrafael/lunarbox/commit/306d6d650bd508ee5e0d7759674ffd389c4afde1))
* working darg and drop! ([bbdc293](https://github.com/Mateiadrielrafael/lunarbox/commit/bbdc293431d8773b1e18563c068ef21e82142d0e))
* wrote lesnes for the Project module ([a2c9e1e](https://github.com/Mateiadrielrafael/lunarbox/commit/a2c9e1ef4030da7cc23462b2bb90b2bfac945556))

## [1.0.1](https://github.com/Mateiadrielrafael/lunarbox/compare/v1.0.0...v1.0.1) (2020-03-18)


### Bug Fixes

* now the page finally renders (or at least I hope so) ([d96c0c3](https://github.com/Mateiadrielrafael/lunarbox/commit/d96c0c346241f107aa511175385605349c8cbe03))

# 1.0.0 (2020-03-18)

### Bug Fixes

- fixed incorrect pnpm config call in the github action and gave the action a better name ([d71cb4f](https://github.com/Mateiadrielrafael/lunarbox/commit/d71cb4fb6cbb9bd237c48ccc573e398cc6a72979))
- fixed the focus issues and deleted text still appearing in function names, also added some comments ([412255e](https://github.com/Mateiadrielrafael/lunarbox/commit/412255e8400a290df182146e12affd69ad56d70a))
- now you can't create duplicate functions ([5d1cd49](https://github.com/Mateiadrielrafael/lunarbox/commit/5d1cd49a41a924eeba4a8823c49d8b4c895e063f))

### Features

- a bunch of css imporvements and some basic validation of the function names ([7176e70](https://github.com/Mateiadrielrafael/lunarbox/commit/7176e7054829aca871951d9932f6a05a8480ce03))
- added a basic editor sidebar ([3555659](https://github.com/Mateiadrielrafael/lunarbox/commit/3555659ffce2fc559e884169d04ba0bae8737648))
- added the emptyEdtiro static component ([c7c65f0](https://github.com/Mateiadrielrafael/lunarbox/commit/c7c65f0fc6d3bb749d67eabe1dc0443439b34a48))
- basic visual function creating ([e624183](https://github.com/Mateiadrielrafael/lunarbox/commit/e624183f7069dd21c87db04330a2ff7f1568e9db))
- basics of the web stuff and routing ([982eea8](https://github.com/Mateiadrielrafael/lunarbox/commit/982eea8f817e5bb6a9bc04e534021cec9a70c8d1))
- basics of typing ([143dac7](https://github.com/Mateiadrielrafael/lunarbox/commit/143dac71eac2300ad01fc1f0d6b1158e7abaedb9))
- better styling for vaildation ([2bd3880](https://github.com/Mateiadrielrafael/lunarbox/commit/2bd388027737a698f9a870a3515c57308fdb9461))
- cancel function creation when clicking outside the input box ([63b9338](https://github.com/Mateiadrielrafael/lunarbox/commit/63b93384df1deb690d68e913de35835a8ba0f399))
- current function highlighting ([1b0aa78](https://github.com/Mateiadrielrafael/lunarbox/commit/1b0aa7819ce9ea50d03339b53678367cebd0accf))
- did some works on Projects ([edcc26a](https://github.com/Mateiadrielrafael/lunarbox/commit/edcc26a825e1c91404b54082dae1f45fd6a16e7b))
- finsihed the type inferring stuff ([5b93c1b](https://github.com/Mateiadrielrafael/lunarbox/commit/5b93c1bdafd839d38aed0f625dc0f6a39923d368))
- more work on Project->Expression converting ([febcaee](https://github.com/Mateiadrielrafael/lunarbox/commit/febcaeed669136b47120ac582a8044f065adb0a2))
- more work on the home page ([a9caf4c](https://github.com/Mateiadrielrafael/lunarbox/commit/a9caf4cf92ed07c51c5c89894e50e050b1427e25))
- native expressions ([0f29aa8](https://github.com/Mateiadrielrafael/lunarbox/commit/0f29aa8aa4515424be3fd4d42ec719e708173eba))
- new functions are now added to the graph and can be "edited" ([52bcdb5](https://github.com/Mateiadrielrafael/lunarbox/commit/52bcdb5a9a3c7e80fcf1fae00807c05eb4cc1694))
- only cancel on input blurs if the config says so ([e796247](https://github.com/Mateiadrielrafael/lunarbox/commit/e7962473e85fa81ada509f6202f91ced9765c62b))
- persistent selections ([0d854c1](https://github.com/Mateiadrielrafael/lunarbox/commit/0d854c1445d803f56eed9830870129c7e02a6b3d))
- POC Expression codegen for debugging purpouses ([8874b2e](https://github.com/Mateiadrielrafael/lunarbox/commit/8874b2ee142636028a7cb38e19b5c3b7a8ae46b0))
- validation for empty function names ([f9f377f](https://github.com/Mateiadrielrafael/lunarbox/commit/f9f377f865b5c1912498706f371a7ff7c85f1f8b))

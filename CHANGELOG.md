# [2.0.0](https://github.com/Mateiadrielrafael/lunarbox/compare/v1.16.2...v2.0.0) (2020-07-26)


### Bug Fixes

* allow adding uncurries custom functions ([769ed15](https://github.com/Mateiadrielrafael/lunarbox/commit/769ed154df014a743aa27569af33a5c33f1f240c))
* automatically update input counts when they change ([d70b1ba](https://github.com/Mateiadrielrafael/lunarbox/commit/d70b1bae2c631218178ec246cb44892ed6701980))
* canvas is no longer squised when a panel is opened ([8944655](https://github.com/Mateiadrielrafael/lunarbox/commit/89446558fc0b35e1e7fd5d3b65628b789f6ef29d))
* color everything on load ([840736a](https://github.com/Mateiadrielrafael/lunarbox/commit/840736a2f8df03e891f159df1b99ea12580d1e3d))
* color the outputs of input nodes as well ([d4f759e](https://github.com/Mateiadrielrafael/lunarbox/commit/d4f759ea0d2f15f3423d243870f99d132c4ded17))
* delete the connection the user clicks on ([cf58c65](https://github.com/Mateiadrielrafael/lunarbox/commit/cf58c65ec4fcf43f240c44cfdb87335ca2e65586))
* display correct info for if expressions ([69a9452](https://github.com/Mateiadrielrafael/lunarbox/commit/69a9452bad81c2e4669411bff45f8f5b7b1c0e12))
* do not generate recursive values for testing ([3c69895](https://github.com/Mateiadrielrafael/lunarbox/commit/3c698951f95dee08c8092b6004f47d4154699624))
* do not move in the opposite direction of the node ([3466e25](https://github.com/Mateiadrielrafael/lunarbox/commit/3466e25d436c653ed34b92e218ef3263c90753df))
* fixed not being to add stuff to main using Enter ([eb0f74a](https://github.com/Mateiadrielrafael/lunarbox/commit/eb0f74ae878c8ce09b3da2ec5bc299fd6ebe4971))
* fixed some typos and grammar issues ([4a825e4](https://github.com/Mateiadrielrafael/lunarbox/commit/4a825e4dded782c9c69bb97908924d7648303866))
* hide the examples tab when there is no example availble ([607cf59](https://github.com/Mateiadrielrafael/lunarbox/commit/607cf593304fbae6f39586088084cb5d3123e375))
* I FINALLY FIXED THIS YAYAYAYAYAYAY ([bc479e5](https://github.com/Mateiadrielrafael/lunarbox/commit/bc479e5d357d16e7e33e44a1a9d5d826ec362779))
* let expressions are generalized properly ([ac3def6](https://github.com/Mateiadrielrafael/lunarbox/commit/ac3def66a218d8a4d1f83335bfbb92e346720c8f)), closes [#42](https://github.com/Mateiadrielrafael/lunarbox/issues/42)
* made negative sqrt null ([6f4b54c](https://github.com/Mateiadrielrafael/lunarbox/commit/6f4b54c9fc21e3dbb9ea168366b9d011a0f5307d))
* move nodes on top when clicking on them ([5c4dab3](https://github.com/Mateiadrielrafael/lunarbox/commit/5c4dab386e851c0b87edbd1ac1227768751eb2cf))
* no longer overlap node previews ([d521165](https://github.com/Mateiadrielrafael/lunarbox/commit/d5211659154d72f9d3ed0d6825d005a61a02ed68))
* no longer recalculate the mouse target on each mouseMove event ([3ed79d1](https://github.com/Mateiadrielrafael/lunarbox/commit/3ed79d1a1c8641ca4308a48579bc9341b32627f3))
* no more labels which should not be there :) ([fb3f1cd](https://github.com/Mateiadrielrafael/lunarbox/commit/fb3f1cdabbfa316b0b180d9874cd46f3ff096935))
* nodes with no input have backgrounds for the text as well ([01a7125](https://github.com/Mateiadrielrafael/lunarbox/commit/01a7125266d187da200038614dff298be53e926f))
* now you can create nodes again ([99bf113](https://github.com/Mateiadrielrafael/lunarbox/commit/99bf113a21cb1b08f73411895494ef37161f1f83))
* only autosave when necessary ([d3f440d](https://github.com/Mateiadrielrafael/lunarbox/commit/d3f440d625f9b92afc2ba7fa5278a37330bdeab2))
* only store the data related to NodeGroups in 1 place ([5340e17](https://github.com/Mateiadrielrafael/lunarbox/commit/5340e177d04b11df50bae4d48587d6ed09787ee6))
* only store user-defined functions in the saved json ([ff69c20](https://github.com/Mateiadrielrafael/lunarbox/commit/ff69c20b9d8f49a46f9b1dfde2caea01b5e21810))
* proper type comparing for tests ([41ea651](https://github.com/Mateiadrielrafael/lunarbox/commit/41ea6515b488c2b5dba66ad9bb0508f1e3b5c0fc))
* properly evaluating after chaning a value in a modal ([a00af31](https://github.com/Mateiadrielrafael/lunarbox/commit/a00af31efec1b5885b313f8b7fee1bed1da0a8dd))
* removed some debug logs ([bd7612d](https://github.com/Mateiadrielrafael/lunarbox/commit/bd7612d00ac4a293d77cd504143cb236c557ef08))
* removed the useless P action I added for debugging ([3a537bd](https://github.com/Mateiadrielrafael/lunarbox/commit/3a537bdbdcc57bf203fdbcd11640df3cf7a5d268))
* rerender previews when types change ([5c5d1be](https://github.com/Mateiadrielrafael/lunarbox/commit/5c5d1be681c016475205f72120920e31e78569c5))
* save the current function ([81d46c0](https://github.com/Mateiadrielrafael/lunarbox/commit/81d46c0e6bb928890367150c0ae1a2b931ca3f98))
* show current number of nodes in project data ([167c20b](https://github.com/Mateiadrielrafael/lunarbox/commit/167c20ba1974bb491396388287eb75a81e8f5ecd))
* show the on hover effect right when unselecting a node ([8f25959](https://github.com/Mateiadrielrafael/lunarbox/commit/8f25959a6f58d3910ebbbef37b9c9ef3dcf0ae0e))
* text is always on top now ([44c8f46](https://github.com/Mateiadrielrafael/lunarbox/commit/44c8f46c905998694051cd76ff8b90d05b8bfbcd))
* the connection preview follows the mouse ([e0153cd](https://github.com/Mateiadrielrafael/lunarbox/commit/e0153cd168fa18cae7ec7e002af5fe9cd19cc868))
* you can add nodes multiple times now ([cf5002c](https://github.com/Mateiadrielrafael/lunarbox/commit/cf5002c23b693d5720e6d839d7d109ef76225522))
* z-order aware node-selection ([fca1e7f](https://github.com/Mateiadrielrafael/lunarbox/commit/fca1e7ff79c22f3a50e8d4855e99cee8e6b4ee29))


### Features

* a basic error panel ([ef01390](https://github.com/Mateiadrielrafael/lunarbox/commit/ef013906e3103009a8818315beffc1886c8c34b2))
* a basic system of returning actions from the ts side to the purs side ([8c45ca7](https://github.com/Mateiadrielrafael/lunarbox/commit/8c45ca7cc49297caf307c0ccaba8194fb9f0484f))
* a basic visual programming linter ([3574486](https://github.com/Mateiadrielrafael/lunarbox/commit/357448668118c1c1e4fd05f70a6d381a7cb7ac3e))
* a button for going back while on the projects page ([639a155](https://github.com/Mateiadrielrafael/lunarbox/commit/639a15527f3c78fd1480a15ccaedde54b2f88dc3))
* a button for hints in tutorials ([3f734c3](https://github.com/Mateiadrielrafael/lunarbox/commit/3f734c3ea040cb9b03b00d0787a850a818b4b862))
* a typeahead for the solution ([de4329c](https://github.com/Mateiadrielrafael/lunarbox/commit/de4329c1f829d6f1047d3c7fe621865cf8710194))
* access to new editor states from the tutorial component ([ff838d7](https://github.com/Mateiadrielrafael/lunarbox/commit/ff838d718f50e5ef9ba322abcc33a16fa4fc53f6))
* actual validation for solutions... for real this time ([16b5df7](https://github.com/Mateiadrielrafael/lunarbox/commit/16b5df7f1b5493dc83198d639deb8c3b11f7937f))
* actually render the previews to the panel ([055f625](https://github.com/Mateiadrielrafael/lunarbox/commit/055f62511dfe16c87b903f6506926197f98bbc0d))
* added a bunch of tooltips ([e6dedfb](https://github.com/Mateiadrielrafael/lunarbox/commit/e6dedfb87a5ea4081b760b8732ea7b110434465c))
* added a way to detect double clicks on nodes ([d58ead7](https://github.com/Mateiadrielrafael/lunarbox/commit/d58ead7aee51131500a6633a295c9675f9ebd4ac))
* added square root node ([7a2bd1e](https://github.com/Mateiadrielrafael/lunarbox/commit/7a2bd1e6fbd18eef897594a8a44b44556e833949))
* align the "go back" button on the projects page with the lists ([affe3f1](https://github.com/Mateiadrielrafael/lunarbox/commit/affe3f10fcbbaf22a565b934f7992eb1f0552456)), closes [#43](https://github.com/Mateiadrielrafael/lunarbox/issues/43)
* always have a current function ([dce09ad](https://github.com/Mateiadrielrafael/lunarbox/commit/dce09ad021cc8420d83f212d40a08c0c8d0503be))
* an error for passing null to a function ([f8bdf86](https://github.com/Mateiadrielrafael/lunarbox/commit/f8bdf866653cb14450ad896cd9b6bdff761740c5))
* api integration for tutorials ([9d4a985](https://github.com/Mateiadrielrafael/lunarbox/commit/9d4a9858754a9350f17404974d01e5eff6d60298))
* basic connection previews ([7906322](https://github.com/Mateiadrielrafael/lunarbox/commit/79063224ec55447a1d16fa307317bc8c08130f4f))
* basic modal system ([0420b04](https://github.com/Mateiadrielrafael/lunarbox/commit/0420b04880dd223c436a55fe627ef2def82442ad))
* basic moving to specific functions when clicking on an errors location ([b080eab](https://github.com/Mateiadrielrafael/lunarbox/commit/b080eab7437c167e8bd841885fbaf38693454217))
* basic node moving and dragging ([f014b13](https://github.com/Mateiadrielrafael/lunarbox/commit/f014b13defdae153340a57e0b023e84dc3588189))
* basic output selecting ([b61ef6b](https://github.com/Mateiadrielrafael/lunarbox/commit/b61ef6b0ad61c0cf13d75d00aeedca7100aabf61))
* basic problems panel ([50b3962](https://github.com/Mateiadrielrafael/lunarbox/commit/50b3962c54afc4db0773ec9b380c84044e1fdd5f))
* basic settings for project visibility + a page to clone any project ([8cbacee](https://github.com/Mateiadrielrafael/lunarbox/commit/8cbacee7466abb066a318d9b8292a26ed7591d1a)), closes [#44](https://github.com/Mateiadrielrafael/lunarbox/issues/44)
* basic tab system ([8cb07ef](https://github.com/Mateiadrielrafael/lunarbox/commit/8cb07ef0f652d79c1e65eeb08204acb556c867ac))
* basic ts stuff ([d450899](https://github.com/Mateiadrielrafael/lunarbox/commit/d450899f6a9f2a5f7b77cdcdcf72bf69837df32e))
* basic tutorials page ([9536cae](https://github.com/Mateiadrielrafael/lunarbox/commit/9536cae95e9d3964cb0f8c0ea66bbefbb93250ba))
* basic types for tutorials ([db3509e](https://github.com/Mateiadrielrafael/lunarbox/commit/db3509ed6e6db55c20031a59a9ab9a426a60f27e))
* basics of node value prevwieing ([d4dd556](https://github.com/Mateiadrielrafael/lunarbox/commit/d4dd5568ad6bc335e5f1244c304958d655d38c0e))
* being able to edit and start tutorials from the projects page ([6a0d064](https://github.com/Mateiadrielrafael/lunarbox/commit/6a0d064e5670ea609d9a08c15bf0ed909bc21b45))
* better error messages ([ebd76f7](https://github.com/Mateiadrielrafael/lunarbox/commit/ebd76f756480904bb42acba9e42f4251c74f733a))
* better expression pretty printing ([181f47c](https://github.com/Mateiadrielrafael/lunarbox/commit/181f47c9fa016ecc624cdc256f3668f59a5b3ce6))
* better looking input fields ([d770be1](https://github.com/Mateiadrielrafael/lunarbox/commit/d770be1ac788e7fce5bcf4eaf1b5a24406fedc20))
* bubbling actions ([7c76005](https://github.com/Mateiadrielrafael/lunarbox/commit/7c7600548241464f54632741626688684f9efc2e))
* capabilities for working with github gists ([eac2b61](https://github.com/Mateiadrielrafael/lunarbox/commit/eac2b618a520f82cea5cf60163b38a68041af090))
* coloring actually works now:) ([54cfe3d](https://github.com/Mateiadrielrafael/lunarbox/commit/54cfe3d3914db99a535eb6bedc49fa594f292715))
* completing tutorials ([9766b4b](https://github.com/Mateiadrielrafael/lunarbox/commit/9766b4bd8728c523e252389776ba7ef12322c996))
* connection deleting ([a7612d0](https://github.com/Mateiadrielrafael/lunarbox/commit/a7612d0963df54840da2524a6f195652a37c9f6c))
* connection picking ([5d68877](https://github.com/Mateiadrielrafael/lunarbox/commit/5d68877c0fc1884cf0fd52ebb66c8a9a8bdc7192))
* creating and deleting tutorials and stuff ([89306aa](https://github.com/Mateiadrielrafael/lunarbox/commit/89306aa84da7b21ff0cb672c615a79a10294e4db))
* creating and rendering connections ([9c1bfec](https://github.com/Mateiadrielrafael/lunarbox/commit/9c1bfec72912dae5b36716c9575aa486cf5d2ffb))
* ctrl+click to edit node ([d834782](https://github.com/Mateiadrielrafael/lunarbox/commit/d8347822a0a22a6488a448414564f8ff71e28ffa))
* dead code elimination ([7822f1f](https://github.com/Mateiadrielrafael/lunarbox/commit/7822f1f855921fd60cf98982dbf9e62c9c060244))
* deleting nodes from the node editing modal ([e3545fc](https://github.com/Mateiadrielrafael/lunarbox/commit/e3545fc19ca82257cfcdbdc60230af29e2776abb))
* deleting tutorials ([66ffce0](https://github.com/Mateiadrielrafael/lunarbox/commit/66ffce09bac9aa6f7c205bf860fb657182f292a8))
* display correct number of inputs when creating a node ([a740ee3](https://github.com/Mateiadrielrafael/lunarbox/commit/a740ee33d9c2f05f24324d85b1686d5a5d06359f))
* displaying quickcheck errors ([f29ccd2](https://github.com/Mateiadrielrafael/lunarbox/commit/f29ccd250b69bdcfa51d94e45fb8627881e84d4a))
* displaying the description of nodes in the node editing modal ([5889016](https://github.com/Mateiadrielrafael/lunarbox/commit/588901670207b392583771bd03b1c70d5a6e027f))
* displaying type errors for tests ([94a7d9f](https://github.com/Mateiadrielrafael/lunarbox/commit/94a7d9f43416e2a27e349b1842017a32f1a3670b))
* expand inputs to see descriptions ([8eadcc2](https://github.com/Mateiadrielrafael/lunarbox/commit/8eadcc2b0c890e7a432cc8cf6c560727fa3780ed))
* expression optimization ([efaba89](https://github.com/Mateiadrielrafael/lunarbox/commit/efaba8967645dcf8a4f3b0043500960aa75c0fc4))
* forgot what I did ([63b548f](https://github.com/Mateiadrielrafael/lunarbox/commit/63b548fbf550ebe1414d697ef339391bff2f33bf))
* function equality ([3836013](https://github.com/Mateiadrielrafael/lunarbox/commit/38360135663c9fa5dfde259604a68147884a4db0))
* generic runtime wrapping system ([b1b02d2](https://github.com/Mateiadrielrafael/lunarbox/commit/b1b02d252fa18036341bf83fa88d44f0abb593b6))
* hover animation for outputs ([0e608a3](https://github.com/Mateiadrielrafael/lunarbox/commit/0e608a36c766c80abfb902c63f25846ef9c23ce8))
* hte ability to run actions inside modals ([b88cb23](https://github.com/Mateiadrielrafael/lunarbox/commit/b88cb23f4ee8b234ec7a37be769380e34fbdec0b))
* implemented a basic node deletion mechanism ([6c5493f](https://github.com/Mateiadrielrafael/lunarbox/commit/6c5493f66441ae23087c34c4a573f156c1dcd8ac))
* implemented basic node previweing ([129f404](https://github.com/Mateiadrielrafael/lunarbox/commit/129f404940ba6b57c49c0f72ec4e95793aa33c1b))
* implemented moving to the location of errors ([76117ce](https://github.com/Mateiadrielrafael/lunarbox/commit/76117ce07b7beeee3ef02fc9232bb1231a950b6f))
* implemented the typescript side of the arc-updating logic ([ad64cce](https://github.com/Mateiadrielrafael/lunarbox/commit/ad64cce6c48571fcd012bcfa103dee0898186c6e))
* input data in the node modals ([f429803](https://github.com/Mateiadrielrafael/lunarbox/commit/f4298034ee0fbf5c6b6a6ef7c1db9d89bfb117ae))
* input field for the gist id used by the tutorial ([2e24cd4](https://github.com/Mateiadrielrafael/lunarbox/commit/2e24cd446797f0f416a53b0d1c7a5e8eee663736))
* interpreting now uses closures ([637bc8a](https://github.com/Mateiadrielrafael/lunarbox/commit/637bc8af45c7b4435c4ab52a5c9785d2fbf6d540))
* lazy if expressions ([cc3a6d8](https://github.com/Mateiadrielrafael/lunarbox/commit/cc3a6d88defe6f174afc7f5bfc1e190f45cada2b))
* made a component for node previews ([76043e7](https://github.com/Mateiadrielrafael/lunarbox/commit/76043e708435c4a866e4fa2d763aee3980406e5d))
* markdown support in tutorials ([96ec89c](https://github.com/Mateiadrielrafael/lunarbox/commit/96ec89c5457c147ee805f2bcac36491994df751d))
* mocked tutorial data ([843cd13](https://github.com/Mateiadrielrafael/lunarbox/commit/843cd133edecaba9b5689fdf038e022c8a827d3d))
* modal childSlot system ([50e2e71](https://github.com/Mateiadrielrafael/lunarbox/commit/50e2e7111836598b87c260a04ccda908bcb3481f))
* more expressive location system ([d868074](https://github.com/Mateiadrielrafael/lunarbox/commit/d8680746e768504da6eebe597fd0c8fa85ea9a15))
* more work on canvas rendering ([7300381](https://github.com/Mateiadrielrafael/lunarbox/commit/73003810d99ac594f689b97f26a229253f5d5b9c))
* more work on detecting what the mouse is over ([265a475](https://github.com/Mateiadrielrafael/lunarbox/commit/265a475c17e59ba5de352a931e9e069112150f50))
* more work on node creating ([22f114f](https://github.com/Mateiadrielrafael/lunarbox/commit/22f114f06ae5edcfbb37458d9bf9862795edc869))
* more work on the hover animation for nodes ([b1a58d4](https://github.com/Mateiadrielrafael/lunarbox/commit/b1a58d4321c97568246f7e0ed25bd6a5ea6bea02))
* more work on waffs issue ([0786ac6](https://github.com/Mateiadrielrafael/lunarbox/commit/0786ac6f9c6aa83547b56039df1b5699fdc76f25))
* MVP for testing user solutions ([2661ab7](https://github.com/Mateiadrielrafael/lunarbox/commit/2661ab7ade0dd54c9128da9a7725756da86e498c))
* name rendering ([53f27ce](https://github.com/Mateiadrielrafael/lunarbox/commit/53f27ce8268b2fdcff558f74b96b1328251c253d))
* new node ui system ([bc8cd56](https://github.com/Mateiadrielrafael/lunarbox/commit/bc8cd56a8281f997d8282977dfaf6a6db3bf8739))
* no more use of explicit graphs for the function dependency graph ([9326983](https://github.com/Mateiadrielrafael/lunarbox/commit/9326983c0d3e0b2356e90374728cdb14cbc312b4))
* node creating and arc refreshing ([ea0e3d0](https://github.com/Mateiadrielrafael/lunarbox/commit/ea0e3d062ebdea4c324e8e7d471e9dcf7bbb63d3))
* node label rendering ([8148255](https://github.com/Mateiadrielrafael/lunarbox/commit/81482551d437824409fc879d30339f3aabe2295a))
* not only editing, but also creating tutorials ([31490fe](https://github.com/Mateiadrielrafael/lunarbox/commit/31490febb87888a86bdfc871456d0d7aadacb1a9))
* now the type checker continues even after finding the first error ([abee9f2](https://github.com/Mateiadrielrafael/lunarbox/commit/abee9f2ee2995e249bfcc489395c398344b85d66))
* on hover animation for inputs ([9541f71](https://github.com/Mateiadrielrafael/lunarbox/commit/9541f71086be82fae96df5c752ae0c603d0279b6))
* on hover animation for nodes ([6ecdfd5](https://github.com/Mateiadrielrafael/lunarbox/commit/6ecdfd5cd4b66ff86a9edf04017dcf81c8dc7e80))
* only allowing certain nodes in tutorials ([d31c309](https://github.com/Mateiadrielrafael/lunarbox/commit/d31c3095e01aef7644f150ca3c3e6a56a537cc16))
* opening finished tutorials in the playground ([20b1faf](https://github.com/Mateiadrielrafael/lunarbox/commit/20b1faf719595c00f39a847535fd0a84c4ba765c))
* prettify json in dev ([b8340a3](https://github.com/Mateiadrielrafael/lunarbox/commit/b8340a3ce532449839854e1d1b79fe4162ae8d40))
* properly arrange inputs when previweing a connection ([6614c87](https://github.com/Mateiadrielrafael/lunarbox/commit/6614c876603a08c12a4c30623a47ba6477b24265))
* properly show the value of a node ([851f3c3](https://github.com/Mateiadrielrafael/lunarbox/commit/851f3c39c7e836e739a19b03f07d85e3f536eeb3))
* quotes locations in formatting ([f756e3b](https://github.com/Mateiadrielrafael/lunarbox/commit/f756e3b250bd313bed61c2ef57ea01324ba570fb))
* readded the search bar ([0b68084](https://github.com/Mateiadrielrafael/lunarbox/commit/0b680847a28a6468c9521653081ac16eb4a5f730))
* recursion works :D ([867b7d2](https://github.com/Mateiadrielrafael/lunarbox/commit/867b7d2c292bc83e1a42430551a2f28b7e350446))
* redesigned the projects page ([eb52c18](https://github.com/Mateiadrielrafael/lunarbox/commit/eb52c1842eb3f8d69b03f3f1b0fc288765e5b43f))
* rendering of inputs for constant nodes ([987722a](https://github.com/Mateiadrielrafael/lunarbox/commit/987722aa19b9e33c21f0975b824f9628821575c7))
* rerender on resize ([95d54cd](https://github.com/Mateiadrielrafael/lunarbox/commit/95d54cd234cf91353f959addf820a2ff7bb7e748))
* setup a tutorial editor component ([6ec4c07](https://github.com/Mateiadrielrafael/lunarbox/commit/6ec4c070b391b4e457462c02d3fde2931219492c))
* show the types of nodes in their modals ([fded7a6](https://github.com/Mateiadrielrafael/lunarbox/commit/fded7a6f9187af985a350823bc6a346351bf8995))
* show the value preview for all nodes ([4b78718](https://github.com/Mateiadrielrafael/lunarbox/commit/4b78718cfc08d98f360b2b941b1d297407189a43))
* show warnings in the problems tab ([0369f55](https://github.com/Mateiadrielrafael/lunarbox/commit/0369f556e281cbda185722fa9e8766b2d83aface))
* show what can and what cannot be connected ([1a95420](https://github.com/Mateiadrielrafael/lunarbox/commit/1a954203d859e1d0b5714cb062d3c462acbba78d))
* showing the runtime overwrites ([1784185](https://github.com/Mateiadrielrafael/lunarbox/commit/17841853a328f516465e9f980224de47d48daab1))
* sketched out the ManageTutorials typeclass ([ce79ab0](https://github.com/Mateiadrielrafael/lunarbox/commit/ce79ab0e88aa0cd41ce41987ae40b4279cfb83b0))
* some work on removing wrong previews ([aac178c](https://github.com/Mateiadrielrafael/lunarbox/commit/aac178c3e601ced03a93108e2c70e1f270b4bf90))
* something way too simple I spent way too much time on ([08da6f9](https://github.com/Mateiadrielrafael/lunarbox/commit/08da6f9a920e4ac7ec73f03b6ea5cf10ca8a7c60))
* super good warning formatting ([634e0b9](https://github.com/Mateiadrielrafael/lunarbox/commit/634e0b9ded35e9dcd7cb694ad34e86021b74bd45))
* system for generating native expressions from purescript values ([c26b69b](https://github.com/Mateiadrielrafael/lunarbox/commit/c26b69bd263f62f514ac854312466d05ba10cbee))
* system of getting types from purescript values ([e9549f5](https://github.com/Mateiadrielrafael/lunarbox/commit/e9549f5579f975326e4e376566f85d1753c0a34a))
* tab selecting ([310e460](https://github.com/Mateiadrielrafael/lunarbox/commit/310e460b976d84f39ecaff6da79d67f5c4cf6f0c))
* text backgrounds ([9fbd42f](https://github.com/Mateiadrielrafael/lunarbox/commit/9fbd42f1046c20cc0d19409bc34d7e303562ea8c))
* the ability to edit nodes by right clicking on them ([8d431ef](https://github.com/Mateiadrielrafael/lunarbox/commit/8d431efced5b69f8918bac448d99e06df51ab8dc))
* the icon of the probelms tab has different colors depening on the project status ([e5f34f9](https://github.com/Mateiadrielrafael/lunarbox/commit/e5f34f9a8a6b10d1db9ad327fcf06d3c3581c6f8))
* The lookup array node ([0b34787](https://github.com/Mateiadrielrafael/lunarbox/commit/0b3478776c07363fec1505668ffae0dedf11af1c)), closes [#41](https://github.com/Mateiadrielrafael/lunarbox/issues/41)
* the lunarbox logo in the editor leads back to the home page ([5037d9a](https://github.com/Mateiadrielrafael/lunarbox/commit/5037d9ae4eacf177216d5db62b6e1026d0766167))
* the renderer can now handle nodes with no output ([a008340](https://github.com/Mateiadrielrafael/lunarbox/commit/a008340e486e8e891f9024a35084d01ac1f1620f))
* tutorial component wich sets up all the data ([bf7fa8d](https://github.com/Mateiadrielrafael/lunarbox/commit/bf7fa8d9ad3242849059a8eb1774b787849c117b))
* tutorial slides rendering ([337c095](https://github.com/Mateiadrielrafael/lunarbox/commit/337c095c0d30c6f9fd170aff30ac21e57e6566b2))
* tutorial validation ([0f0d9e4](https://github.com/Mateiadrielrafael/lunarbox/commit/0f0d9e406e29e57bdf285cdb3acb681e212b1727))
* typeahead for selectnig what project a tutorial is based on ([638c800](https://github.com/Mateiadrielrafael/lunarbox/commit/638c800a0717c5ad95eeb44dbb718f3b9a9df3b3))
* unselect pins ([f3350fb](https://github.com/Mateiadrielrafael/lunarbox/commit/f3350fbd0fd8400b78aec5b82dd49ec59d58eb55)), closes [#40](https://github.com/Mateiadrielrafael/lunarbox/issues/40)
* updated the arc positioning algorithm to place all the arcs on the minimum amount of layers ([abd9208](https://github.com/Mateiadrielrafael/lunarbox/commit/abd92081d4bd3fec029ddfc9c44d4824418ffe82))
* very basic geometryCache saving / loading ([2332ad2](https://github.com/Mateiadrielrafael/lunarbox/commit/2332ad21ee74915b85bf7a164a253e9df5be3fd4))
* very basic input rendering ([b1419af](https://github.com/Mateiadrielrafael/lunarbox/commit/b1419af2e728815357dd52fbb3775cb8684fcc93))


### Performance Improvements

* removed unnecessary reevualating ([ac02737](https://github.com/Mateiadrielrafael/lunarbox/commit/ac0273733061899a75a9ef7c53c2d462e340edf7))


### BREAKING CHANGES

* the save structure stores the current function directly instead of a Maybe.

## [1.16.2](https://github.com/Mateiadrielrafael/lunarbox/compare/v1.16.1...v1.16.2) (2020-05-08)


### Bug Fixes

* removed extra / from prod api link ([a2a2b00](https://github.com/Mateiadrielrafael/lunarbox/commit/a2a2b005f5ead320d08cac895b1ca151765ea6c6))

## [1.16.1](https://github.com/Mateiadrielrafael/lunarbox/compare/v1.16.0...v1.16.1) (2020-05-08)


### Bug Fixes

* correct api prod url ([1e9b960](https://github.com/Mateiadrielrafael/lunarbox/commit/1e9b960d25c41117404b4e41b7d8899010311735))

# [1.16.0](https://github.com/Mateiadrielrafael/lunarbox/compare/v1.15.0...v1.16.0) (2020-05-08)


### Bug Fixes

* display admin settings when logged in as an admin ([8f6236f](https://github.com/Mateiadrielrafael/lunarbox/commit/8f6236f026917318462c80e9c260e167874968d6))
* no more autosaving while editing the name ([e411706](https://github.com/Mateiadrielrafael/lunarbox/commit/e411706f407f1522eddd971f0b11b081c4dfe6dc))


### Features

* autosave ([b8dfcf0](https://github.com/Mateiadrielrafael/lunarbox/commit/b8dfcf03e94bda254389bb9ecdb8104733b505ce))
* better projects page on smaller windows ([273a2d6](https://github.com/Mateiadrielrafael/lunarbox/commit/273a2d68df4797ab21bedf67bf21c34b1c482306))
* cloning examples ([53c0d5e](https://github.com/Mateiadrielrafael/lunarbox/commit/53c0d5eb387d4975090a141aaafbdc51a359eddc))
* error page ([3f4d086](https://github.com/Mateiadrielrafael/lunarbox/commit/3f4d0869dcaafb1e67fca0f83796e404f419a65d))
* loading animation ([fe79bdb](https://github.com/Mateiadrielrafael/lunarbox/commit/fe79bdb973fb571d903219bbc19c01da7139a944))
* more array nodes ([eefcd52](https://github.com/Mateiadrielrafael/lunarbox/commit/eefcd529ce870c6a2edb8470c667c1bfc4fcc93b))
* more array nodes ([29aec1e](https://github.com/Mateiadrielrafael/lunarbox/commit/29aec1e75295e1af7ca3318d55247d5979f77400))
* project deleting ([bbd0531](https://github.com/Mateiadrielrafael/lunarbox/commit/bbd0531d2897efbe058427766dcef3c3fac21d44))

# [1.15.0](https://github.com/Mateiadrielrafael/lunarbox/compare/v1.14.0...v1.15.0) (2020-05-07)


### Bug Fixes

* clean up dependency graph when deleting nodes ([09c30c8](https://github.com/Mateiadrielrafael/lunarbox/commit/09c30c8366a592afa2d7778cfd94486a15fa2b68))
* fixed function level dependency cycles when adding nodes from the search bar ([098ebe4](https://github.com/Mateiadrielrafael/lunarbox/commit/098ebe4b96da38344d62d4761272af19ae81a11a))


### Features

* added a favicon and an icon in the left bottom corner of the editor ([25cbc8f](https://github.com/Mateiadrielrafael/lunarbox/commit/25cbc8f4141ad3224274617467133c63191a8ad4))
* descriptions for all the nodes ([16e0d8d](https://github.com/Mateiadrielrafael/lunarbox/commit/16e0d8da84bd266cd6d0427f86a886dcd0318442))
* display pin descriptions ([98b1721](https://github.com/Mateiadrielrafael/lunarbox/commit/98b1721882b497dfdf44c9762824d52ebbac17d7))
* even more keyboard shortcuts ([824ec32](https://github.com/Mateiadrielrafael/lunarbox/commit/824ec3221ca48260e2c7d204be4f9910005e356b))
* getting the project list from the api ([f7c6ddd](https://github.com/Mateiadrielrafael/lunarbox/commit/f7c6ddda9a1187146010f2e031012228e2a56e52))
* int id instead of strnig ([c8226c5](https://github.com/Mateiadrielrafael/lunarbox/commit/c8226c5eaaa9b293571b7c58ad4a7719737e7b1c))
* loading projects from the api ([1197273](https://github.com/Mateiadrielrafael/lunarbox/commit/119727307e016f247cbe350a396ead87595f7bdc))
* more keybindings for the add nodes input ([2d95dee](https://github.com/Mateiadrielrafael/lunarbox/commit/2d95dee827718601b15d1d740d8fed209889f1a6))
* more math nodes ([2fbdd20](https://github.com/Mateiadrielrafael/lunarbox/commit/2fbdd208fec4ff3a8a14f3d847e8533ce53cbe71))
* node deleting ([fcdce2b](https://github.com/Mateiadrielrafael/lunarbox/commit/fcdce2b1b62b68c02a1db5dfad0a6bbad4759dc3))
* project creating ([65f683b](https://github.com/Mateiadrielrafael/lunarbox/commit/65f683b21af8bdf424a968a612e353bdd7c3d54b))
* spawn output nodes in the middle of the screen ([00b4ce2](https://github.com/Mateiadrielrafael/lunarbox/commit/00b4ce2a33c37c3b500c9a16e26a8f85ba04c147))

# [1.14.0](https://github.com/Mateiadrielrafael/lunarbox/compare/v1.13.0...v1.14.0) (2020-05-06)


### Bug Fixes

* fixed a tiny bug which made it so you had to double click to remove connections ([9a79ea6](https://github.com/Mateiadrielrafael/lunarbox/commit/9a79ea6d503eef54d22facb12f6f8a1565609270))
* no longer require double clicking on stuff to connect it at times ([c096c97](https://github.com/Mateiadrielrafael/lunarbox/commit/c096c97fd95e62e2d8cd085fcfee6511cf7e4c9b))
* take as much space as possible for ndoe data on the Add panel ([579e04f](https://github.com/Mateiadrielrafael/lunarbox/commit/579e04f64ddf8af4244d910530a1fd3f961bea27))


### Features

* basic arrays ([3147e40](https://github.com/Mateiadrielrafael/lunarbox/commit/3147e408ae0a568f5dad1c62318890d897ff3cfd))
* better array printing ([3b338db](https://github.com/Mateiadrielrafael/lunarbox/commit/3b338db3d56461aebd94f22e1a60b1ce9b92317f))
* better home page ([e22cb3f](https://github.com/Mateiadrielrafael/lunarbox/commit/e22cb3fc0d568f1845f8d250b0be1983f002793e))
* node searching ([3fa00f3](https://github.com/Mateiadrielrafael/lunarbox/commit/3fa00f3984ebf9e7a98c23294601415579afceea))
* nodes for comparing ([2879712](https://github.com/Mateiadrielrafael/lunarbox/commit/28797124618d57fccb905a121245ff0545e1f0de))
* removed useless literal expressions ([2f5d480](https://github.com/Mateiadrielrafael/lunarbox/commit/2f5d48068129eff4bff50ce1c64f04b94b41dd9e))
* require auth for some routes ([4a74cc4](https://github.com/Mateiadrielrafael/lunarbox/commit/4a74cc417161bf909d5137d7c59af58ce8eec0f2))
* typed holes ([5efa777](https://github.com/Mateiadrielrafael/lunarbox/commit/5efa777f22a7f210fc5b0c645451f4ea1b133a01))

# [1.13.0](https://github.com/Mateiadrielrafael/lunarbox/compare/v1.12.0...v1.13.0) (2020-05-06)


### Bug Fixes

* fixed opening a second project without refreshing breaking the svg ([b347d0a](https://github.com/Mateiadrielrafael/lunarbox/commit/b347d0a0ea1e36dc50562828e9e1424dc4c554fb))
* now the cursor is set to text on all inputs ([79e35fb](https://github.com/Mateiadrielrafael/lunarbox/commit/79e35fb87dedd5c6bec1047398a23fe39eab34f0))


### Features

* basic projects page component ([a61e83b](https://github.com/Mateiadrielrafael/lunarbox/commit/a61e83b567d5ace053f3c183ee03221ac0563b19))
* clamped zooming ([66fce14](https://github.com/Mateiadrielrafael/lunarbox/commit/66fce1456211332c8809699adc3ff0da101c9919))
* function level cycle prevention ([e968b90](https://github.com/Mateiadrielrafael/lunarbox/commit/e968b90b75a2b10144202eb4ba91901db01b094f))
* in function cycle prevention ([31e811d](https://github.com/Mateiadrielrafael/lunarbox/commit/31e811d4e3f56a78a96cd73908c28fd885cb1d1e))
* inputs names on hover ([af3afc2](https://github.com/Mateiadrielrafael/lunarbox/commit/af3afc2800028d69c002c181df6b801f9e54432e))
* modified the editor state encoding and decoding to better fit the api ([33a3d24](https://github.com/Mateiadrielrafael/lunarbox/commit/33a3d245db48d2c3a1167c258fb0f10c9878b595))
* only be able to connect pins of correct types ([0f519e7](https://github.com/Mateiadrielrafael/lunarbox/commit/0f519e71a35f7541c7edeee1b98aadfd580ecbca))
* project saving ([ac79b53](https://github.com/Mateiadrielrafael/lunarbox/commit/ac79b534b90f18c9e29b98193e31c4cf887b8321))
* project searching ([7b82511](https://github.com/Mateiadrielrafael/lunarbox/commit/7b82511d9caf1d38e28dc71ec890bf12797e7f58))
* same for outputs ([333f72d](https://github.com/Mateiadrielrafael/lunarbox/commit/333f72d0ab1db6aacf9ebf4796a9c380206c9846))
* see node output on hover ([545bf65](https://github.com/Mateiadrielrafael/lunarbox/commit/545bf65bdc74c5042a6d70aae26c1b7a29957c72))
* setting the names and example status of projects ([7cbb25f](https://github.com/Mateiadrielrafael/lunarbox/commit/7cbb25ff92d1820cc23e8a4810a88cccc58793a8))
* settings page ([8fc1b42](https://github.com/Mateiadrielrafael/lunarbox/commit/8fc1b426924b63464655b47c8db972b6703d194d))
* some logic related to loading any Project ([601f8b7](https://github.com/Mateiadrielrafael/lunarbox/commit/601f8b7d2dcb62e87a604440142ad4a136c1643d))
* some styling on the projects page ([c64ef9f](https://github.com/Mateiadrielrafael/lunarbox/commit/c64ef9f50e28d040ecf8e8eacc101828811761ec))
* visually show what inputs cant be connected ([b0a8d83](https://github.com/Mateiadrielrafael/lunarbox/commit/b0a8d83bc51cff259d611bea149d2596552f0087))

# [1.12.0](https://github.com/Mateiadrielrafael/lunarbox/compare/v1.11.0...v1.12.0) (2020-05-02)


### Bug Fixes

* fixed a giant bug in the type inference algorithm - now generaliation works properly ([8c2287a](https://github.com/Mateiadrielrafael/lunarbox/commit/8c2287aba2d8aa0903c3c4c363834e771d03b4a5))
* fixed page reloading on any form ([ddb3dfd](https://github.com/Mateiadrielrafael/lunarbox/commit/ddb3dfdb10ae34c17dc542b04e77a553786543f4))
* fixed purescript code not starting on load ([3c13fba](https://github.com/Mateiadrielrafael/lunarbox/commit/3c13fba5504035784219c2f4266b021b49634a08))
* fixed type-inference for let-bindings ([2aff9a8](https://github.com/Mateiadrielrafael/lunarbox/commit/2aff9a8eae9aac65d1520239f0edc1a453ed5146))
* no longer display nulls ([04d9cbb](https://github.com/Mateiadrielrafael/lunarbox/commit/04d9cbb0e1162dacb2f16c188b8a93e9526a8834))
* now the connection offset to the result doesn't apply on the mouse as well ([c26b927](https://github.com/Mateiadrielrafael/lunarbox/commit/c26b927c710ba7c86fef817ccc9e1e159d92acd1))
* the editor now resizes properly ([6ffdf73](https://github.com/Mateiadrielrafael/lunarbox/commit/6ffdf73f94f25d77e03572d3a596f1984ea63322))


### Features

* added a basic camera structure ([7d628c2](https://github.com/Mateiadrielrafael/lunarbox/commit/7d628c299ddf79f5fd077d18b1ab1393d890f40d))
* adding inputs ([49c0206](https://github.com/Mateiadrielrafael/lunarbox/commit/49c0206bacf796f20112499ad2ffb1e4f99f51c6))
* basic cameras ([5a25864](https://github.com/Mateiadrielrafael/lunarbox/commit/5a25864769f6480c16527b25f3ef15b84f7709a6))
* basic json encoding / decoding ([59c7477](https://github.com/Mateiadrielrafael/lunarbox/commit/59c7477cd114d665d7a117c486c73028966e7e6f))
* better validation for passwords ([aa32738](https://github.com/Mateiadrielrafael/lunarbox/commit/aa327387d2ed5ea139d81a34c5c93653188d70b9))
* currying ([d6fabec](https://github.com/Mateiadrielrafael/lunarbox/commit/d6fabec0bae61eb6998791b9694f7075b84cb4e7))
* custom inputs ([6145448](https://github.com/Mateiadrielrafael/lunarbox/commit/61454487d88a56ccb31033af7c6a41ecea22e1b2))
* do not navigate to a login or register form when logged in ([692eec6](https://github.com/Mateiadrielrafael/lunarbox/commit/692eec62b70b24c401ea2a8f8ff597bf56703d40))
* fixed some routing problems and created more:( ([138419f](https://github.com/Mateiadrielrafael/lunarbox/commit/138419f5c3c5d155e6f0bd1a164bb64f12c5b5fe))
* interpret some stuff as fixpoints ([037361f](https://github.com/Mateiadrielrafael/lunarbox/commit/037361f591144032b5c2b990e4434c35fb479388))
* logging in ([ae5b50b](https://github.com/Mateiadrielrafael/lunarbox/commit/ae5b50b06c870d6df26b9548aeb4b824a87bb1ad))
* logging in with the actual api ([4187da3](https://github.com/Mateiadrielrafael/lunarbox/commit/4187da3030e351358d09883854c71ebf4e585303))
* login form ([9d78177](https://github.com/Mateiadrielrafael/lunarbox/commit/9d78177bd62227a69b0ecfcbdb31d2af68a410ba))
* more function based nodes and fixed some visual bugs ([893bc94](https://github.com/Mateiadrielrafael/lunarbox/commit/893bc94e0b6f8026984a0e2ff428a24fff6dd4f9))
* nodes are added to the center of the screen ([a584cdb](https://github.com/Mateiadrielrafael/lunarbox/commit/a584cdb35bcaa61dfab1f9fb14675a4961e1229e))
* not showing the expression on top of nodes anymore ([e0beddc](https://github.com/Mateiadrielrafael/lunarbox/commit/e0beddc1ba210472a2e5e5a827c9c395583efb10))
* now you can see the entire type on top of ndoes, not only the output ([99773aa](https://github.com/Mateiadrielrafael/lunarbox/commit/99773aae01c3c92fba83b1cb0361e5c5aa95edec))
* number helpers ([23f10a3](https://github.com/Mateiadrielrafael/lunarbox/commit/23f10a3d8fb70b3d9c6e5bd5efe16030876b6bc8))
* number node ([cd0937d](https://github.com/Mateiadrielrafael/lunarbox/commit/cd0937d6451920e16781c6a5fe4f6291439eee1a))
* pannig ([944383c](https://github.com/Mateiadrielrafael/lunarbox/commit/944383c80c4ee44bb1d82eec1a1b26b2202b4624))
* pushing nodes on top when dragging ([3fd3f96](https://github.com/Mateiadrielrafael/lunarbox/commit/3fd3f96824adcb67c4467ab4aeaf7bd6f3619ad3))
* register form ([82577c2](https://github.com/Mateiadrielrafael/lunarbox/commit/82577c2e1ea4b5d75a2a25251896e3ca38a462fa))
* saving of runtime overwrites ([993203d](https://github.com/Mateiadrielrafael/lunarbox/commit/993203d9b822857e6577362e993aa410fc2a97f2))
* showing types using arrows ([ae7eaef](https://github.com/Mateiadrielrafael/lunarbox/commit/ae7eaef1e18ebed041414e1f75826d6bd2c8d005))
* some logic for logging in ([9333b9d](https://github.com/Mateiadrielrafael/lunarbox/commit/9333b9dc27aafa754016f49009c87c8a4ecb9515))
* string helpers ([4ea7244](https://github.com/Mateiadrielrafael/lunarbox/commit/4ea7244e884f7c460fb96cbe32d3188d5aa80e1e))
* string node ([dbe2d96](https://github.com/Mateiadrielrafael/lunarbox/commit/dbe2d9672e92e79966668b2e88dcd4c680192158))
* styled number nodes ([9be4a89](https://github.com/Mateiadrielrafael/lunarbox/commit/9be4a895f18a38181b7274b6cc0066439dbafb48))
* styled scrollbars and adding inputs ([da9182d](https://github.com/Mateiadrielrafael/lunarbox/commit/da9182d93e3ccc9605a0d5da1ca7e5a372aa4b28))
* the entry file is now written in typescript ([186eedf](https://github.com/Mateiadrielrafael/lunarbox/commit/186eedf236edeb1f2b34095997eea5bf500522b7))
* trim node ([2699c62](https://github.com/Mateiadrielrafael/lunarbox/commit/2699c623ab9a750f07d06478fb8894fec40fc257))
* use the right cursor for nodes and stuff ([88062d0](https://github.com/Mateiadrielrafael/lunarbox/commit/88062d028f2d578e2b3ec1e72d5673aa03bd55ee))
* very basic login form ([e055281](https://github.com/Mateiadrielrafael/lunarbox/commit/e0552817d0d3d5499f51c3dbb2be764d6d6778de))
* very basic user stuff ([9c52ca3](https://github.com/Mateiadrielrafael/lunarbox/commit/9c52ca3e99e83a93d78627d935773a0c9c73149f))
* zooming ([640b8f2](https://github.com/Mateiadrielrafael/lunarbox/commit/640b8f25fc0edd69d40414c2b2ffaeddf676e349))


### Performance Improvements

* floor everything ([7a7f7f4](https://github.com/Mateiadrielrafael/lunarbox/commit/7a7f7f468ca9b17d50b27c730a6ae2697730c9f9))

# [1.11.0](https://github.com/Mateiadrielrafael/lunarbox/compare/v1.10.0...v1.11.0) (2020-04-28)


### Bug Fixes

* a few performance optimizations and fixed some values not changing ([45c1280](https://github.com/Mateiadrielrafael/lunarbox/commit/45c128075ea1223537bf920350039c037222be03))
* fixed overflow issues with long function names ([67427b3](https://github.com/Mateiadrielrafael/lunarbox/commit/67427b3cfd9ded59cad68ae6ac612f88a4147b3e))
* fixed overflow scrolling issues for too many functions in the tree view ([ebb0f5c](https://github.com/Mateiadrielrafael/lunarbox/commit/ebb0f5ced7b693f80345a2eed08620aa7a1f0500))
* fixed overflow scrolling issues on the add page ([7b6306a](https://github.com/Mateiadrielrafael/lunarbox/commit/7b6306af962fb27a1230c479124cfa4a96b708f0))
* now the switch component is in state with the runtime value overwrites ([01ad12a](https://github.com/Mateiadrielrafael/lunarbox/commit/01ad12aef3452f5c505ea377ab85030bc1992311))
* the switch component now syncs with the state ([cbf8da7](https://github.com/Mateiadrielrafael/lunarbox/commit/cbf8da73bfff6efcf382d55b088263eab3421253))


### Features

* ctrl+b to togglet the panel on the left ([9bb6d22](https://github.com/Mateiadrielrafael/lunarbox/commit/9bb6d22ac16a497d5a01ae75e083d1fff367766e))
* custom node-specific ui ([d7c008d](https://github.com/Mateiadrielrafael/lunarbox/commit/d7c008daca9a4c33947b690fca4ad742802650f2))
* doing some cleanup when replacing a connection ([2ad6b28](https://github.com/Mateiadrielrafael/lunarbox/commit/2ad6b285edd028f3f97a6cf7f4740608bbed48fa))
* logic for node deleting ([c5376a5](https://github.com/Mateiadrielrafael/lunarbox/commit/c5376a52f47a716baa545b73089b598017112298))
* logic gates ([085a33a](https://github.com/Mateiadrielrafael/lunarbox/commit/085a33ad9b6eca85690f28fb21a40629840ad383))
* node deleting ([219de5a](https://github.com/Mateiadrielrafael/lunarbox/commit/219de5a041a42fab27787d9265c160db17f9c4c7))
* some ffi code to allow using mausetrap-ts from purescript ([ef9b848](https://github.com/Mateiadrielrafael/lunarbox/commit/ef9b848140eef0fd59120ba930ce7d5df2736862))
* types for function uis ([71e3477](https://github.com/Mateiadrielrafael/lunarbox/commit/71e3477307a581054eff96646f97c712570aac4a))


### Performance Improvements

* do not requery the dom for the scene every render ([55ec7c4](https://github.com/Mateiadrielrafael/lunarbox/commit/55ec7c4ab353617db98643a42a90ddfd82e70e8b))
* some more optimizations ([5c33543](https://github.com/Mateiadrielrafael/lunarbox/commit/5c33543f757375cca0b46bb482d65e48603a9c44))

# [1.10.0](https://github.com/Mateiadrielrafael/lunarbox/compare/v1.9.0...v1.10.0) (2020-04-27)


### Bug Fixes

* fixed bugs in the runtime ([54fc101](https://github.com/Mateiadrielrafael/lunarbox/commit/54fc101ef3950de2b3d5cb2ecc745c862cabe88b))
* now its a tiny bit easier to connect stuff ([d7913b8](https://github.com/Mateiadrielrafael/lunarbox/commit/d7913b809e1cc1d05ab7ebec2042ff3627cf942d))
* now new functions start with an output node ([3576566](https://github.com/Mateiadrielrafael/lunarbox/commit/357656626c5a5669cd4d331fede6c4fd444b2602))
* now yuo can move stuff without creating connections ([756a0d5](https://github.com/Mateiadrielrafael/lunarbox/commit/756a0d54061ef6185bf595713aab9d176305021f))
* working topological sorting ([3c5baab](https://github.com/Mateiadrielrafael/lunarbox/commit/3c5baabfdde124bdc5615042ced12ee48bdeb375))


### Features

* 3 new native functions exposed: identity, const and pipe ([ee9bf09](https://github.com/Mateiadrielrafael/lunarbox/commit/ee9bf0906282a86660f7bc4a3cc1358e5b543cc4))
* connection preview for everything ([1d5b02e](https://github.com/Mateiadrielrafael/lunarbox/commit/1d5b02e631c4aaf972a93887ee1deffc70d142f8))
* connection prview for outputs ([bd70211](https://github.com/Mateiadrielrafael/lunarbox/commit/bd70211288d24b20f63141840695b002156391c3))
* connection removing ([75164b7](https://github.com/Mateiadrielrafael/lunarbox/commit/75164b7c17f52827522dcf8e5ada0f8e7de0868e))
* executing... kinda ([ec3f979](https://github.com/Mateiadrielrafael/lunarbox/commit/ec3f979793817a96b894985ff59b9800f80c788f))
* interpreting ([e5bc256](https://github.com/Mateiadrielrafael/lunarbox/commit/e5bc2560b875cea513b295cea0c26aa8ea9322ea))
* polished connection previews ([9e5cabe](https://github.com/Mateiadrielrafael/lunarbox/commit/9e5cabe17f502b2890f17049fb2bf25902592a6a))
* scale the connections on hover ([3083044](https://github.com/Mateiadrielrafael/lunarbox/commit/3083044c4e1d9ee8c3de29775bf25d2968d738c9))
* the Interpreter monad ([677b96a](https://github.com/Mateiadrielrafael/lunarbox/commit/677b96a3d388fc63e7b50d54b5e862c50c11ecc9))
* the logic of edge removing ([f07c32d](https://github.com/Mateiadrielrafael/lunarbox/commit/f07c32d19f3e567bdb0faba67965d17d0dad6bd3))
* types for everything runtime related ([b4caf0b](https://github.com/Mateiadrielrafael/lunarbox/commit/b4caf0bf40c307837ba89f4bfc97a88ef09a1458))

# [1.9.0](https://github.com/Mateiadrielrafael/lunarbox/compare/v1.8.0...v1.9.0) (2020-04-26)


### Bug Fixes

* fixed the fillWith function always returning stuff aligned with the y axis ([ac4916a](https://github.com/Mateiadrielrafael/lunarbox/commit/ac4916a9fe86a851654d2a0a80c260e9a0fcf0a4))
* now the connected node inputs rotate towards the output ([1cd4655](https://github.com/Mateiadrielrafael/lunarbox/commit/1cd465593ca61bbdca5248b434962a0bfc66502c))


### Features

* function arguments coloring ([766745e](https://github.com/Mateiadrielrafael/lunarbox/commit/766745eba63c1b21fab00d59956f5e3ab7ff0c67))
* multi-layer node inputs ([ac243a4](https://github.com/Mateiadrielrafael/lunarbox/commit/ac243a42a6aef42228852304e868d6e23c44fe59))
* visual connecting ([f19797a](https://github.com/Mateiadrielrafael/lunarbox/commit/f19797ae903b31a6c44c164406c59c3ac2b04a01))

# [1.8.0](https://github.com/Mateiadrielrafael/lunarbox/compare/v1.7.0...v1.8.0) (2020-04-23)


### Features

* A function to add a connection to a state ([9e0a425](https://github.com/Mateiadrielrafael/lunarbox/commit/9e0a42511e6631b21d15ee9103ec55a32216c34b))
* basic types and stuff for PartialConnections ([f3f8fbf](https://github.com/Mateiadrielrafael/lunarbox/commit/f3f8fbf34f471ccb95f96b75d8ec76d51c44c0a5))
* CONNECTIONS!!! ([e7fd0dc](https://github.com/Mateiadrielrafael/lunarbox/commit/e7fd0dc81a0b8365a335ada7f42e32c8c19a63df))
* display the types on the Add panel ([c500728](https://github.com/Mateiadrielrafael/lunarbox/commit/c500728d5e1ae3bfe6541d72e5063d8182c81e23))
* experimental connections ([f95e1ef](https://github.com/Mateiadrielrafael/lunarbox/commit/f95e1efba47fa294af8c82251fc53d168abad559))
* expression optimization ([57e4175](https://github.com/Mateiadrielrafael/lunarbox/commit/57e417582143c0d5e540c9f8bb78862d93ef57f9))
* instead of increasing the radius of node inputs now I increase the stroke-width ([9ea094d](https://github.com/Mateiadrielrafael/lunarbox/commit/9ea094d5d20c4bd9a3ef3be472f6a23390f11064))
* making node inputs bigger on hover ([6604ff1](https://github.com/Mateiadrielrafael/lunarbox/commit/6604ff1c3aeb7ba0b0fad61b4c8e97c343bd0313))
* pretty printing of expressions ([410cb4b](https://github.com/Mateiadrielrafael/lunarbox/commit/410cb4bccfd9f7fd92d5d7bc6c8447886e165fab))
* scaling of outputs on hover ([825dd81](https://github.com/Mateiadrielrafael/lunarbox/commit/825dd81a8d028ff111aba2f57ad0540035fdd3ad))
* syntax highlighting everywhere!!! (and fixed wrong output types) ([02070b4](https://github.com/Mateiadrielrafael/lunarbox/commit/02070b4f3ffc93a6489f1b5612fd8f0ac0f7a62e))
* type syntax highlighting ([c669772](https://github.com/Mateiadrielrafael/lunarbox/commit/c66977295994f37a7f6ed7c13da175858eed7f15))

# [1.7.0](https://github.com/Mateiadrielrafael/lunarbox/compare/v1.6.0...v1.7.0) (2020-04-22)


### Bug Fixes

* correct linecaping for nodes without inputs ([7bdb410](https://github.com/Mateiadrielrafael/lunarbox/commit/7bdb4106744956087de6ca39a5ff7e60ac3b39d4))
* FINALLY FIXED TYPES NOT APPEARING ON TOP OF NODES ([b0bf340](https://github.com/Mateiadrielrafael/lunarbox/commit/b0bf340fbd2322b13923df1aeb7b80b3c765918f))
* fixed compilaion results only containing output nodes ([a38790a](https://github.com/Mateiadrielrafael/lunarbox/commit/a38790ac628362fdf991cd85bf1754aba6b314c4))
* fixed infinite recursion when detecting an intersection ([a03ecd7](https://github.com/Mateiadrielrafael/lunarbox/commit/a03ecd73062d033e80368b534da0ffa87f361d28))
* fixed partial initial loading of native functions ([84b7335](https://github.com/Mateiadrielrafael/lunarbox/commit/84b73356bb6c89c07b188cdc02dbf7b709c73b44))
* fixed some input pins having the wrong color ([44e4f1e](https://github.com/Mateiadrielrafael/lunarbox/commit/44e4f1eb7b40e94d0c576270f445fad1531738a1))
* fixed some random problems with node compiling ([81cf7fd](https://github.com/Mateiadrielrafael/lunarbox/commit/81cf7fd5a3ecabd944f0d0ddfe85bdaf7d0cb683))
* for now unkown colors just become gray ([01f01cd](https://github.com/Mateiadrielrafael/lunarbox/commit/01f01cdd1f009ed412a2ac62378bb910bedcf130))
* now you can drag nodes even from the empty spaces between input arcs ([5e24521](https://github.com/Mateiadrielrafael/lunarbox/commit/5e2452144787c047bfe6a45fcd51bb744ea18f55))
* removed debug logs ([804b101](https://github.com/Mateiadrielrafael/lunarbox/commit/804b10119e87da07cec34d06af8ea617ccf56237))


### Features

* arc overlapping solver ([4cab578](https://github.com/Mateiadrielrafael/lunarbox/commit/4cab578b1a22c419197c8adbe1e612ba3f8db493))
* colored node inputs ([f8f2809](https://github.com/Mateiadrielrafael/lunarbox/commit/f8f2809f109ed018c49898ad9e89c76406387669))
* colored outputs ([eddd2b3](https://github.com/Mateiadrielrafael/lunarbox/commit/eddd2b33b2df3591d3f9755a4670c422a86e6893))
* colored types on the Add panel ([364ce5b](https://github.com/Mateiadrielrafael/lunarbox/commit/364ce5b5555a39b0372dd15010e18006a403ce4e))
* display errors with a nice illustration ([97cd0fc](https://github.com/Mateiadrielrafael/lunarbox/commit/97cd0fc58f12f3e208194845976258a52805ec63))
* dotted lines ([77ca8d7](https://github.com/Mateiadrielrafael/lunarbox/commit/77ca8d7f484a3dfa3c4fec8a55b49123812213e0))
* empty space filling alg for arcs on a circle ([67e37a6](https://github.com/Mateiadrielrafael/lunarbox/commit/67e37a62828d5050034b7ffca21b518b8a09e3d1))
* free type variable finding for the Scene component ([7b934df](https://github.com/Mateiadrielrafael/lunarbox/commit/7b934df584351fb90be837316a1c43ffbc5714f4))
* got dnd back ([b4c0bc9](https://github.com/Mateiadrielrafael/lunarbox/commit/b4c0bc9d0abd508bdb7bb883bf519a63472dba01))
* helper to draw chords ([26abe47](https://github.com/Mateiadrielrafael/lunarbox/commit/26abe477ff5826e84cb4892e6e560887c4f9b4b8))
* only compile when necessary ([ca719cc](https://github.com/Mateiadrielrafael/lunarbox/commit/ca719cce17761e91d1fee285a0efbd5c43df2da8))
* random colors for unkown types ([cbdedf0](https://github.com/Mateiadrielrafael/lunarbox/commit/cbdedf01cbda04753b05b5ff35a752abed4c7f74))
* rendering for node inputs! ([e505b1d](https://github.com/Mateiadrielrafael/lunarbox/commit/e505b1d1a84a67f9cdf7e99a9bc929ab56c570a0))
* some more coloring stuff ([2ff7081](https://github.com/Mateiadrielrafael/lunarbox/commit/2ff70816f9fda4a120f4f6475f6a0cc661823e4f))
* started work on pin locations ([c6a1747](https://github.com/Mateiadrielrafael/lunarbox/commit/c6a1747b22a2fac8398130be8d9e199d5372f2b9))
* type prettifying ([0996d07](https://github.com/Mateiadrielrafael/lunarbox/commit/0996d072198708e4a392787e301cc438e72e0cfd))

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

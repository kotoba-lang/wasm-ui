# kotoba WASM UI

[![CI](https://github.com/kotoba-lang/wasm-ui/actions/workflows/ci.yml/badge.svg)](https://github.com/kotoba-lang/wasm-ui/actions/workflows/ci.yml)

This project hosts the kotoba DOM-compatible WASM UI substrate and the browser
reference renderers:

- `wasm-ui`: WebGL retained-tree renderer with text canvas overlay
- `wasm-webgpu`: WebGPU retained-tree renderer with text canvas overlay

The substrate keeps Reagent/re-frame-shaped CLJS code portable by providing
small compatibility namespaces in `src/reagent/core.cljc` and
`src/re_frame/core.cljc`. UI is lowered into a kotoba virtual document, encoded
as the `kotoba:dom` host ABI, retained by renderer hosts, then projected to draw
ops for WebGL/WebGPU.

```sh
npm install
npm run check:wasm-ui

npm run compile:wasm-ui
npm run compile:wasm-webgpu
python3 -m http.server 8701 -d public
# http://localhost:8701/kotoba-wasm-ui.html
# http://localhost:8701/kotoba-wasm-webgpu.html
```

Browser debug hooks:

```js
kotoba.wasm.demo.debug_snapshot()
kotoba.wasm.demo_webgpu.debug_snapshot()
```

Coverage entry points:

```sh
clojure -M:test -n kotoba.wasm.compat-api-test
clojure -M:test -n kotoba.wasm.dom-compat-test
clojure -M:test -n kotoba.wasm.abi-runtime-test
clojure -M:test -n kotoba.wasm.debug-test
clojure -M:test -n kotoba.wasm.retained-host-test
clojure -M:test -n kotoba.wasm.wit-contract-test
clojure -M:test -n kotoba.wasm.golden-test
clojure -M:test
```

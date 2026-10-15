# The browser build gets an 8 MiB shadow stack

wasm-ld gives the linear-memory stack 1 MiB, and the browser build has no
native stack guard, so a recursion that did not fit trapped with
"memory access out of bounds". The parser's precedence ladder is ~30 frames per
nesting level, and in an unoptimized `wasm-pack --dev` build
`await (^2).map({ start { 1 } })` overflowed it. The JS wrapper then hid the
trap behind a second one (`finally` freeing an unset pointer), so the symptom
pointed at `__wbindgen_free`. `.cargo/config.toml` now links wasm32 builds with
`-zstack-size=8388608`.

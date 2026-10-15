# The browser build runs programs again

`Interpreter::new` reads the wall clock for the local UTC offset through
`epoch_nanos`, which called `SystemTime::now()` directly. That panics on
wasm32-unknown-unknown ("time not implemented on this platform"), so every
`evaluate` call in the browser package died with `unreachable` before running
a statement, and the nightly `wasm-e2e` job (`site/concurrency.test.mjs`)
failed all 28 checks. On wasm `epoch_nanos` now answers the host clock plus the
cooperative scheduler's virtual offset, the same source `time`, `now` and
`DateTime.now` use.

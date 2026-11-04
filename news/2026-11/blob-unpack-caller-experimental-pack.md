# `Blob.unpack` runs the caller's `EXPERIMENTAL-PACK`

Core's `Blob.unpack` is experimental, but rakudo runs the `EXPERIMENTAL-PACK`
routine visible in the calling scope when there is one (`nqp::getlexcaller`),
splitting a template string into directives first. P5pack installs that
routine with `BEGIN OUR::<EXPERIMENTAL-PACK> := &f`, so `use P5pack` is what
makes `$buf.unpack($template)` legal. mutsu only knew the `use experimental
:pack` pragma and refused every other call.

The refusal now applies only when no such routine is visible. P5pack's seven
test files match rakudo (`t/spec-unpack.rakutest` was 0 of 24).

# `Blob() :$key!`-style coercion of an Array now works

Triaging `Crypt::RC4`'s test suite (a row in
`todo/tickets/dist-test-suite-failures-batch.md`) found that a `Blob()`
coercion parameter given an `Array` argument (`submethod TWEAK(Blob()
:$key!)` called with `my uint8 @passphrase = ...`) died with "Impossible
coercion from 'Array' into 'Blob': no acceptable coercion method found" —
even though calling `Blob.new(@array)` directly works fine.

The coercion fallback in `try_coerce_value_with_method`
(`src/runtime/types/coercion.rs`) only tries a target type's
`.new(positional)` constructor when the target is a *user*-registered class
(present in `registry().classes`). `Blob`/`Buf` (and their typed variants
like `blob8`, `Buf[uint8]`) are native types built by a dedicated
`build_native_buf_value` helper with no `registry().classes` entry, so the
fallback never reached them, regardless of the target actually being
constructible from a positional list. Fixed generally: when the coercion
target is `is_native_buf_constructible`, build it via the same native buf
constructor the explicit `.new()` path already uses.

This was one of two independent bugs blocking `Crypt::RC4`'s suite; see
`todo/deep/mark-context-flags-leak-across-live-call-boundary.md` for the
remaining one.

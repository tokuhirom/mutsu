# Fast method-dispatch cache silently dropped attributive-parameter writes (Cro::HTTP suite fully green, 35/35)

The compiled-method fast-dispatch cache (`try_populate_fast_cache`,
`vm_call_method_compiled_cache.rs`) is keyed by `(class, method)` only and,
once populated on the first call to a method (a cache miss), routes every
later call through `call_compiled_method_fast` — a path that bypasses
`env_mut()` entirely for speed and populates locals directly from source
data.

The slow path (`call_compiled_method`) has an explicit gate refusing the fast
path for a method with an attributive parameter (`$!x`/`@!a`, Raku's
"bind straight to an attribute" parameter syntax), because such a parameter
mutates `self` and needs `mirror_attributive_params_to_cell` to push the
bound value into the instance's shared attribute cell after argument
binding. `try_populate_fast_cache` had a second, independently-maintained
copy of the same `has_complex_params` eligibility check, and that copy never
got the attributive-parameter exclusion. So a method like:

```raku
class Box {
    has Supply $!s;
    method set-s(Supply $!s --> Nil) { }
}
```

got cached as fast-dispatchable. `call_compiled_method_fast` never mirrors
an attributive parameter into the instance's cell, so every call after the
first silently dropped the argument — the attribute stayed at its
declaration-time seed (the bare type object, here `Supply`) forever.

The bug was invisible through the most natural reproduction shape (`$obj.
method(arg)` with `$obj` a plain named variable) because a variable receiver
compiles to `CallMethodMut`, which calls `call_compiled_method` directly and
never consults the fast cache at all. It only showed up when the receiver
was a fresh expression re-evaluated each call — an array/hash subscript, a
sub call — resolving to a *different* instance on the second-and-later
call. This is exactly `Cro::HTTP2::GeneralParser`'s
`%streams{$curr-sid}.message.set-body-byte-stream($body.Supply)` shape (one
`Stream` per HTTP/2 multiplexed request, looked up by id), which is why the
symptom only reproduced inside the real Cro file and resisted five prior
from-scratch minimization attempts — the minimized repros all bound the
receiver to a local first.

Fix: add the same `attr_twigil_base` exclusion to `try_populate_fast_cache`'s
`has_complex_params` check, mirroring the existing gate in
`call_compiled_method`. Pinned by
`t/fast-method-cache-attributive-param-writeback.t` (array-subscript,
hash-subscript, and sub-call receivers; both untyped and typed attributive
params), all verified against `raku` first.

With this, `Cro::HTTP2::GeneralParser`'s 2-concurrent-stream HTTP/2 parsing
works correctly and the **Cro::HTTP suite reaches 35/35 fully green**
(`http2-request-parser.rakutest` 61/61, up from 60/61) — closing out the
last open item from the multi-month Cro::HTTP compatibility campaign.
Cro::Core stays at 9/9. `make test` is fully green (3113 files, 28897
tests).

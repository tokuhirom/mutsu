# A `whenever` callback body was compiled from AST on every emitted value

`eval_block_value_inner` caches the bytecode it compiles for a carrier block,
keyed by the code object's `SubData.id` plus the ambient compile context. But it
*bypassed* that cache whenever any of four per-code-object mutations had to be
applied to the freshly compiled chunk -- the supply-body mark, the emitter name,
the vouched capture set, and the inherited owned-lexical set:

```rust
let needs_fresh_mutation = is_supply_block_body
    || supply_emitter_sym.is_some()
    || !supply_authoritative_free_vars.is_empty()
    || !whenever_inherited_owned.is_empty();
```

The stated reason was sound: a cached `Arc` must stay byte-identical to what a
fresh compile would have produced, and these four fields are what the uncached
path mutates afterwards. The conclusion was not. All four are derived from the
**code object being run**, not from the ambient frame, so for one cache id they
are the same on every call. They are a *key*, not a reason to bypass.

A `whenever` callback's `authoritative_captures` is never empty, so it took that
branch on **every emitted value**. One full `Compiler::compile` over the
callback's AST per item through the supply -- and with it, one full `core::fmt`
Debug traversal of that AST for each `function_body_fingerprint` the compiler
computes along the way, which costs more than the compilation it is identifying.

The four values are part of `CarrierCompileCtxKey` now, and applied to the fresh
chunk before it is shared, so the byte-identity argument still holds and the
cache serves.

## Results

A supply whose `whenever` callback runs once per emitted value, counted with the
new `carrier-compile: hits=.. misses=..` vm-stat:

| emits | misses before | misses after |
| --- | --- | --- |
| 10 | 12 | 3 |
| 40 | 42 | 3 |

Exactly one compile per value before; a constant after.

On the HTTP/2 request parser (release build, this container):

| | before | after |
| --- | --- | --- |
| DATA frame | 2.12 ms | 1.23 ms |
| HEADERS frame | 13.7 ms | ~11 ms |

## The motivating case now passes

`cro-http`'s `t/http2-request-parser.rakutest` under `MUTSU_REAL_TEST=1` -- item 3
of the vendored-`Test` battery gate, [#7555](https://github.com/tokuhirom/mutsu/issues/7555),
and the reason [#7667](https://github.com/tokuhirom/mutsu/issues/7667) was filed:

| | passes |
| --- | --- |
| before any of this work | 0 / 15 |
| after the registry-COW fix (#7786) | 3 / 15 |
| after sharing the program tables (#7796) | 15 / 20 |
| after the scoped candidate-match overlay (#7800) | 16 / 20 |
| after this change | **29 / 30** |

The test loses a race whose two sides start at the same instant: one waits for a
DATA frame and then reads `*.body-blob.result`, the other runs three `ok`
assertions. Every fix in that list moved the losing side specifically -- the DATA
frame and `body-blob`, not the `ok`s -- which is why they moved the outcome where
a uniformly faster interpreter would not have.

`tests/carrier_compile_cache_serves_whenever_callbacks.rs` pins the miss count
against the emit count; `t/whenever-callback-recompile-semantics.t` pins what the
cached chunk must still do -- chained supplies keep their own emitters, captured
lexicals accumulate, `state` persists, a nested `whenever` registers per value,
and `LAST` still runs.

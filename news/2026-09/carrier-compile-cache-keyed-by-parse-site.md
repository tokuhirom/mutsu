# The carrier compile cache was keyed by the wrong identity

`eval_block_value_inner` caches the bytecode it compiles for a carrier block so
the same block does not have to be re-compiled from AST on every call. It keyed
that cache on `SubData.id`.

`SubData.id` is `next_instance_id()` — a fresh number for every `Sub` *value*.
So the cache worked only for a block whose code object outlives its calls. A
block **instantiated more than once from the same literal** got a new key every
time: it could never hit, and it left a permanently unreachable entry behind in
a map with no eviction.

That is not an exotic shape. `Cro::MessageWithBody.body-blob` is

```raku
Promise(supply {
    my $joined = Buf.new;
    whenever self.body-byte-stream -> $blob { $joined.append($blob); LAST emit $joined }
})
```

so every call builds a fresh supply block — and paid three full
`Compiler::compile` runs plus three dead cache entries for it, every time.

## What the key should be

The compiled chunk is a pure function of the body AST plus the ambient context
already recorded in `CarrierCompileCtxKey`. The right identity is therefore the
**parse site**, and mutsu already has one: `CompiledCode::closure_body_arc`
builds one `Arc<Vec<Stmt>>` per `stmt_pool` slot and hands every later
instantiation of that literal an `Arc` bump of it. Pointer identity of that
`Arc` *is* "the same literal".

`CarrierCacheKey::Site` holds that `Arc` — not just its address, so the
allocation cannot be freed and a later one reused underneath a stale entry, the
same soundness argument `MapGrepCacheKey` already makes. `CarrierCacheKey::Id`
keeps the old behaviour for the call sites that legitimately have their own
stable id to key on (the regex code-block and `s///` replacement paths, whose
ids name a parsed node rather than a value).

## The half that was hiding behind it

Keying by parse site alone only got the *supply block body* to hit; the
`whenever` callback still missed. `split_whenever_body_phasers` partitions a
`whenever` body into its main statements and its `LAST`/`QUIT` phaser bodies, and
it ran on every **registration**, deep-cloning every statement into fresh `Vec`s.
So the callback `Sub` was handed a brand-new `Arc` each time and the cache could
not see it as the same block — and the O(body) AST clone was pure waste besides.

The split is a pure function of the body, so it is memoized on the same
parse-site identity now (`WheneverBodySplit`), and `exec_whenever_scope_op`
passes the pool-owned `Arc` rather than a borrowed slice.

## Results

Carrier compile misses for one `supply { whenever … }` literal instantiated N
times:

| instantiations | before | after |
| --- | --- | --- |
| 10 | 30 | 3 |
| 40 | 120 | 3 |

Per-instantiation before, a constant after — and the unbounded cache growth goes
with it.

The `Promise(supply { whenever … })` shape itself, with Cro's module stack
loaded, goes from 1.55 ms to 1.17 ms per call (25%); `body-blob.result` on the
HTTP/2 request parser from 2.2 ms to ~1.9 ms.

## What this does *not* fix

It does not make `cro-http`'s `t/http2-request-parser.rakutest` stable under
`MUTSU_REAL_TEST=1`, and the measurement is worth recording so nobody assumes it
did. That test loses a race whose losing side is `DATA frame → body-blob → one
ok`; measured against a real-`Test` `ok` at 0.083 ms, the winning side costs
about 0.25 ms and the losing side about 3.1 ms. The margin comes entirely from
what the main thread does after the promise is kept, and under CPU contention it
is a coin flip: 20/20 idle but 9/20 with three cores busy, both before and after
this change.

Closing that needs the losing side under about 1 millisecond, which means the
other half of the cost: env copy-on-write deep copies that scale with the size of
the program (7,253 env entries copied per DATA frame, ~17,000 per `body-blob`).
See [#7667](https://github.com/tokuhirom/mutsu/issues/7667).

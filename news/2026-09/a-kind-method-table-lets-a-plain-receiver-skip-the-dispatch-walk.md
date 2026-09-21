# A `(kind, method)` table lets a plain receiver skip the dispatch walk

`@a.elems` on a plain array cost **7,894 instructions**. The operation itself —
read the backing vector's length, box it as an `Int` — is about two hundred of
those. The other 7,700 are the walk to it.

That walk is three prologues in a row. `try_native_method_raw` asks about
thirty-odd questions of the receiver before it reaches the arity-keyed cascade:
is it a `Seq` that needs touching, a lazy pipe, a `Proxy`, a `Buf` being
written, an `Instance` whose class overrides this name, a collection holding an
`Instance` that has to render itself. Then `native_method_0arg` asks its own
set: a lazy `Match`, a `.VAR` descriptor, the `Unicode` package, a `Scalar`
container, a `Mixin`, a `Uni`, a `Version`. Then `dispatch_core` asks a third:
`Date`/`DateTime` attributes, a RakuAST node, `Instant`, `Duration`, `Buf`. Only
then do the eight method families that answer the call run.

Every one of those questions re-decodes the same NaN-box tag — 57 decodes for
one `@a.elems` — and re-compares the same method name against its own list of
special cases.

## None of them can claim it

Each probe is gated on one of two things: a method *name* from a short fixed
list, or a receiver *kind*. For a plain array calling `elems`, every gate fails
on one or the other. Nothing about that depends on the particular array, the
particular call site, or anything that changes at run time. It is a fact about
the source of the three prologues, and it is the same fact on iteration 1 and
iteration 100,000.

So it is now decided once, in a table. `src/builtins/fast_0arg.rs` maps a
`(receiver kind, method symbol)` pair to a single verdict: *the family cascade
alone decides this call*. When the pair is in the table, the VM's native entry
jumps straight to the families and the three prologues never run.

## What the table is not

It does not reimplement any method. It authorizes a *skip*; the answer still
comes from the same `dispatch_core_families` cascade that produced it before, so
each method's semantics stay in exactly one place. An entry can only be wrong by
authorizing a pair that some skipped guard would have claimed — never by
computing a different answer than the cascade would.

That failure mode has a net under it. Debug builds re-answer every authorized
call through the full path and assert the two agree, so a later commit that adds
a guard invalidating an entry fails the TAP suite (CI's `gc-stress-tap` and
`jit-stress-tap` jobs build debug) instead of silently returning the wrong
value. The entry conditions themselves are written out in the module's docs: the
exact name lists the three prologues gate on, so adding an entry is a check
against a list rather than a re-reading of 2,900 lines.

The receiver half is deliberately narrow. `Value::dispatch_shape` is a pure tag
probe that answers `Some` for a plain non-shaped non-lazy array, a non-itemized
hash, and a `Str` — and `None` for everything the gauntlet exists for:
`Instance`, `Package`, `Mixin`, `Scalar`, `Seq`, `LazyList`, `Proxy`, a lazy
`Match`, a shaped or lazy array, an itemized hash. A shaped array still reports
its shape, a lazy one still throws `X::Cannot::Lazy`, a `Range` still counts its
span.

The one question the table cannot answer is whether user code has `augment`ed
the receiver's builtin type with a method of this name — that is a property of
the registry, not of the call — so the fast lane still consults the same
memoized `native_lever_a_user_override_sym` gate the general path uses.
`augment class Array { method elems { 999 } }` still wins.

## Measured

Paired callgrind runs, `--profile profiling`, second run of each (the first
after a build recompiles the module under test):

| | before | after | |
| --- | ---: | ---: | ---: |
| `@a.elems` × 100k | 1,007,436,905 | 842,849,179 | **-16.3%** |
| `$s.chars` × 100k | 862,705,097 | 697,817,490 | **-19.1%** |
| `%h.elems` × 100k | 999,149,291 | 837,661,103 | **-16.2%** |
| `$o.m()` × 100k (user class — no entry applies) | 2,396,283,983 | 2,395,495,635 | -0.03% |
| bare loop skeleton | 218,060,472 | 218,071,339 | +0.005% |
| `bench-ctor.raku` | 1,578,522,639 | 1,562,366,580 | **-1.0%** |
| `bench-array.raku` | 240,262,059 | 240,357,140 | +0.04% |
| `bench-hash.raku` | 229,497,798 | 229,511,346 | +0.006% |
| `bench-json-fast.raku` | 2,184,818,429 | 2,186,948,600 | +0.10% |

With the loop skeleton subtracted, the per-call figures are the ones that
matter: `@a.elems` **7,894 → 6,248** (-20.9%), `$s.chars` **6,446 → 4,797**
(-25.6%), `%h.elems` **7,811 → 6,196** (-20.7%).

The three small positives are the honest cost of the change: a receiver whose
kind or method the table does not cover now pays one tag probe and one integer
compare before the ordinary walk. They are at or below this box's run-to-run
spread, and `bench-json-fast` in particular neither gains nor loses meaningfully
— its hot receivers are not in the seed set.

## What this does not fix

A method call is still 6,248 instructions where rakudo does one in roughly
thirty cycles. This removes one of the two structural costs
[#8888](https://github.com/tokuhirom/mutsu/issues/8888) identified — the walk in
front of the answer — and leaves the other: the call protocol itself
(`exec_call_method_mut_op_impl`, `exec_one_dispatch`, `LocalKey::with`,
`try_compiled_method_mut_or_interpret_sym`), which is about 3,800 instructions
and belongs to the per-call-site inline cache tracked as
[#8880](https://github.com/tokuhirom/mutsu/issues/8880). The two compose: an
inline cache in front of the call needs a sound miss path behind it, and this
table is what makes the miss cheap. #8888 stays open.

The table's seed set is `elems`/`end`/`Bool` on an array, `elems`/`Bool` on a
hash, `chars`/`Bool` on a `Str`. Widening it is now a one-line addition per
pair, checked against the name lists in the module's docs and guarded by the
debug cross-check — which is the point of building the mechanism before the
coverage.

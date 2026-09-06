# A bare literal picks the boxed candidate where rakudo picks the native one

## Repro

```raku
multi sub d(int $x) { "native" }
multi sub d(Int $x) { "boxed" }
say d(5);
```

* rakudo: `native`
* mutsu: `boxed`

The variable-shaped spellings are already right — `my int $n = 5; d($n)` answers
`native` and `my Int $b = 5; d($b)` answers `boxed`, both in mutsu and in
rakudo, and they are pinned by `t/multi-resolve-cache-keys.t`. Only the bare
literal diverges.

## Re-measured 2026-09-06 — it is the whole native family, and the rule is
## LITERAL provenance, not "fits the native width"

The original note guessed the fix as "any boxed `Int` that fits the native width
must rank `int` at least as specific as `Int`". **That is measurably wrong** and
would trade one wrong answer for another. Measured against raku v2026.07:

| Call | raku | mutsu |
|---|---|---|
| `d(5)` | `native` | **`boxed`** |
| `d(5 + 0)` | `native` | **`boxed`** |
| `d("7".Int)` | `boxed` | `boxed` — correct |
| `d(2**70)` | `boxed` | `boxed` — correct |

`5 + 0` answers `native` because rakudo constant-folds it back to a literal;
`"7".Int` produces an in-range boxed `Int` at runtime and answers `boxed`. So
the discriminator is **where the value came from**, not what it holds — and a
rule based on the value alone gets `"7".Int` wrong.

It is not specific to `Int` either. The same divergence, same shape:

| Call | raku | mutsu |
|---|---|---|
| `h(1e0)` with `num`/`Num` candidates | `nativenum` | **`boxednum`** |
| `i("a")` with `str`/`Str` candidates | `nativestr` | **`boxedstr`** |
| `j(5)` with `int64`/`Int` candidates | `i64` | **`boxed`** |

And two rows that bound the rule — mutsu already agrees on both, and a fix must
keep them:

| Call | raku | mutsu |
|---|---|---|
| `m(5)` with `uint`/`Int` candidates | `boxed` | `boxed` |
| `k(5)` with `int`/`Any` candidates | `native` | `native` |

So an integer literal's native type is exactly `int` (`= int64`): `int64` is an
exact match, `uint` is not, and `Any` still loses to it. Declaration order does
not matter (`e(5)` with the candidates reversed still answers `native`).

## Why it happens, and why it is not a ranking-table edit

`type_hierarchy_distance_with_var_type` (`src/runtime/dispatch_candidates.rs`)
**already** implements the ranking correctly — exact native match at 0, its
boxed equivalent at 1 — but only when the argument carries a `var_type`, which
`unwrap_varref_for_dispatch` reads off a `VarRef` wrapper. A literal has no
source variable, so `var_type` is `None` and the generic hierarchy distance
ranks `Int` at 0.

Fixing the ranking is therefore not the work; **getting literal provenance to
the dispatcher** is. The candidate search runs at runtime and sees only a plain
`Value`, while "this argument position was written as a literal" is a
compile-time fact.

## Suggested shape, and the cost to measure first

The call-site descriptor already exists: `add_arg_sources_constant`
(`src/compiler/mod.rs`) bakes one entry per argument position, with a small set
of sentinel shapes (`NIL` = no source, `TRUE` = a `|EXPR` spread, `Str(name)`
and `Pair(name, slot)` = an rw source variable). A fifth shape naming the
position's literal native type would carry exactly what is missing, and
`unwrap_varref_for_dispatch` could then hand `type_hierarchy_distance_with_var_type`
the `var_type` it already knows what to do with.

**Measure before committing to that.** The constant is currently elided
entirely when every entry is `NIL` (`entries.iter().all(|v| v.is_nil())`), which
is the overwhelmingly common case; marking literals would materialize it at a
large fraction of all call sites. That is constant-pool growth and a decode on
a hot path, paid by every program, to fix a case that only shows up when a
`multi` declares both the native and the boxed candidate. Take the counters
(`MUTSU_VM_STATS`, and `MUTSU_ALLOC_STATS=1` with the `alloc-stats` feature) on
a representative program before and after, and consider restricting the marking
to call sites whose callee is a `multi` if the cost is real.

## Where

* `src/runtime/dispatch_candidates.rs` — `unwrap_varref_for_dispatch`,
  `candidate_type_distance`, `type_hierarchy_distance_with_var_type`
  (which is already right, given a `var_type`), `is_native_type_name`,
  `native_to_boxed`.
* `src/compiler/mod.rs` — `add_arg_sources_constant`, and every decoder of that
  constant (`decode_arg_sources` / `decode_arg_slip_positions`), which must
  tolerate the new shape.

## How it was found

While pinning the multi-resolution cache keys
(`news/2026-09/multi-resolve-cache-keys-carry-definedness-and-declared-type.md`).
It is pre-existing and independent of the cache: the same wrong answer comes out
of a single cold call with no cache involved.

# A bare integer literal picks the `Int` candidate where rakudo picks `int`

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

## Why it happens

An integer literal arrives at dispatch as a plain boxed `Int` value with no
source variable, so `unwrap_varref_for_dispatch` reports no declared type and
`candidate_type_distance` ranks `Int` at distance 0 and `int` further away.
Rakudo instead treats an integer literal as a native `int` for dispatch
purposes: its literal is an `IntLexRef`/native constant, so the native
candidate is the exact match.

The fix is in the *ranking*, not in the caches: a `Value::Int` that came from a
literal (or, more simply, any boxed `Int` that fits the native width) must rank
`int` at least as specific as `Int`. Getting that right needs a look at
`candidate_type_distance`'s native-type rows and at what else keys off "no
declared type", so it is not a one-liner.

## Where

* `src/runtime/dispatch_candidates.rs` — `unwrap_varref_for_dispatch`,
  `candidate_type_distance`, `type_hierarchy_distance`.

## How it was found

While pinning the multi-resolution cache keys
(`news/2026-09/multi-resolve-cache-keys-carry-definedness-and-declared-type.md`).
It is pre-existing and independent of the cache: the same wrong answer comes
out of a single cold call with no cache involved.

# The method-exit merge asked the expensive question first

`bench-ctor`'s round 7 (issue #7568). Rounds 5 and 6 each found the same shape
of bug — work that a flat profile hides because it is spread across
`malloc`/`hashbrown`/`memcmp` rather than concentrated in one hot symbol. This
round's is a third instance, and the cheapest yet to fix: **`merge_method_env`
ran its string-predicate battery before the O(1) test that drops most of the
keys it was asked about.**

Every non-pure method return merges the callee's scoped env overlay back into
the caller's. A nested method call *flattens* that overlay, so the callee's
overlay carries a copy of every parent lexical and global — `%*ENV`, `@*ARGS`,
`$*OUT`, `$*CWD`, the type names in scope, `=pod`, the caller's own lexicals.
The filter that decides what merges had two independent `return None` arms:

1. **the frame-key predicate** — is this key frame state (a param, a compiled
   local, `self`/`?CLASS`, an attribute twigil form, an index-rw temp, a
   `__mutsu_type::` marker, ...) that must not leak to the caller? Every arm
   needs the key back as a *string*: a `Symbol::as_str()` thread-local round
   trip, then a scan of the frame's params/locals/`env_only_decls` by name, plus
   a live attribute-map read guard for the twigil check.
2. **the inherited-entry test** — is the caller already holding exactly this
   value? One `Env::get_sym` (Symbol-keyed) plus `cheaply_unchanged`, an O(1)
   pointer/scalar compare.

(1) ran first, so every inherited entry paid the whole battery before (2) threw
it away. On `benchmarks/bench-ctor.raku` the overlay carries ~24 keys per method
call and (2) alone accounts for ~19 of them.

Both arms `return None`, so which fires first cannot change the outcome for any
key — only what it costs to reach it. Swapping them is a pure reordering.

## Measured

Whole-bench instruction count, `benchmarks/bench-ctor.raku` (5000
constructions, release, callgrind — this container has no `perf`, and
callgrind's counts are deterministic, which is what a perf iteration wants
anyway): **1,464,985,648 -> 1,371,782,759, −6.4%** (293k -> 274k instructions
per construction).

The three symbols that carried the removed work all left the profile's top
list: `call_compiled_method_fast::{{closure}}` (the predicate body, 3.07%),
`Symbol::with_str` (1.22%) and `__memcmp_avx2_movbe` (1.53%).

Interleaved same-session wall-clock A/B (release builds of `main` and of this
change, alternating, `taskset -c 2`, best of 9):

| benchmark | before | after |
|---|---|---|
| `bench-ctor`  | 0.417s | 0.404s (−3.1%) |
| `bench-class` | 0.268s | 0.263s (−2.0%) |
| `method-call` | 0.243s | 0.222s (−8.5%) |

The wall delta is smaller than the instruction delta because what went away was
cache-friendly (repeat scans over strings already resolved and hot). This is not
a `bench-ctor`-specific fix: every non-pure method return in every program pays
this merge, which is why `method-call` moves the most. Wall-clock confirmation
belongs to the bench CI (`bench-history.tsv` on `bench-data`), per the ticket's
measurement notes.

## Lesson, alongside rounds 5 and 6

Rounds 5 and 6 warned that a flat profile can hide a per-call *compile* and
per-call *name re-derivation*. This one adds: it can also hide a **filter whose
arms are in the wrong order**. Nothing here was doing unnecessary work in
isolation — every predicate was needed by some key — but 80% of the keys were
answered by the arm that ran last. When a `filter_map` has several independent
early-outs, order them by cost, and say so in a comment so the next edit does
not silently undo it.

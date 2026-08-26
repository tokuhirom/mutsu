# A `for` loop's multi-param chunk size counts a trailing slurpy param as arity, but rakudo does not

A pointy-block signature for a `for` loop that mixes a mandatory positional
param with a trailing slurpy (`-> $a, *@rest { ... }`) picks the wrong number
of elements to consume per iteration.

## Minimal repro

```raku
for 1, 2, 3, 4 -> $a, *@rest { say "$a : @rest[]" }
```

* `raku`: consumes **one** element per iteration (`@rest` is always empty):
  ```
  1 :
  2 :
  3 :
  4 :
  ```
* `mutsu` (current `main`, independent of the statement-modifier fix that
  found this): does not even parse the header as a working `for` loop —
  `parse_for_params`'s comma-multi-param branch mishandles the leading `*` on
  a later parameter, so the whole line falls through to something else
  entirely and prints four bogus "Useless use of constant integer ... in sink
  context" warnings instead of running a loop at all.

The equivalent closure used as a `for` *statement modifier* operand (fixed in
`news/2026-08/pointy-block-arity-in-for-statement-modifier.md`, which lowers
the closure's own signature into `Stmt::For`'s `params`/`params_def` fields
directly, bypassing the broken text-based header parser above) does parse and
run, but still consumes the **wrong** number of elements:

```raku
say (-> $a, *@rest { "$a:@rest[]" } for 1, 2, 3, 4);
```

* `raku`: `(1: 2: 3: 4:)` (one element per iteration, `@rest` always empty).
* `mutsu`: `(1:2 3:4)` (two elements per iteration -- `@rest` gets one element).

## Root cause

`src/compiler/stmt.rs`'s `Stmt::For` compilation computes the per-iteration
chunk size as:

```rust
let arity = if !params.is_empty() {
    params.len() as u32
} else {
    1
};
```

This counts **every** entry in `params`, including a trailing slurpy one. But
rakudo's for-loop chunking is keyed on the number of *required* (non-optional,
non-slurpy) params, with a minimum of 1 -- exactly the `required_arity`
computation already used a few lines below for the "too few positionals" guard
(`params_def.iter().filter(|d| d.default.is_none() && !d.optional_marker)`,
which does not exclude `slurpy` either, so it likely has the same gap for a
slurpy-only trailing param). The `arity` used for chunking should use the same
"required" notion, not raw `params.len()`.

Note this is a *narrower* case than the one already handled correctly:
`-> $a, $b = 9 { ... }` (an **optional-with-default**, non-slurpy, trailing
param) DOES chunk by the full `params.len()` in rakudo (confirmed empirically:
`for 1,2,3 -> $a, $b = 9 {}` chunks in twos, with the last chunk short one
element triggering the default). So the fix is not "chunk by required count
always" -- it is specifically that a **slurpy** trailing param does not count
toward the chunk size (default/optional non-slurpy params still do), while
still binding `_.elems`-based default logic for any non-slurpy optional
params. This distinction is exactly why the fix needs care rather than a
blanket `params.len()` -> `required_arity` swap.

## Why it's a separate ticket

This is a `Stmt::For` compiler bug independent of, and pre-dating, the
statement-modifier arity fix that surfaced it. Fixing it means reworking the
chunk-size formula in `src/compiler/stmt.rs` (the `let arity = ...` computation
and probably the parallel `required_arity` guard) with care not to regress the
already-correct optional/default trailing-param case, plus separately fixing
`parse_for_params`'s text-based header parser
(`src/parser/stmt/control/for_params.rs`) so `for LIST -> $a, *@rest { }`
parses as a loop at all instead of falling through to garbage. Two separate,
non-trivial fixes -- out of scope for the statement-modifier ticket that found
them.

## Affected files

* `src/compiler/stmt.rs` -- the `let arity = ...` chunk-size computation.
* `src/parser/stmt/control/for_params.rs` -- `parse_for_params` /
  `parse_destructuring_or_plain_param` / `parse_for_pointy_param`, whose
  comma-multi-param branch does not accept a slurpy (`*@rest`) later parameter.

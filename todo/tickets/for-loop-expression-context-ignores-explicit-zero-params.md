# A `for` loop used in expression (value-collecting) context ignores `-> {}`'s zero-arity guard

A `for` loop with an explicit zero-parameter pointy block (`-> {}`) is
supposed to throw if the source has any elements at all -- rakudo invokes the
block once per element and the zero-arity signature immediately rejects the
first argument. Mutsu enforces this correctly when the loop is a plain
statement, but silently ignores it when the same loop is used as an
**expression** (its results collected into a value, e.g. inside `say (...)`).

## Minimal repro

```raku
my $i = 0;
say (for 1, 2, 3, 4 -> { $i++ });
```

* `raku`: dies immediately —
  `Too many positionals passed; expected 0 arguments but got 1`.
* `mutsu`: prints `(0 1 2 3)` -- the zero-arity block is happily invoked once
  per element, `$i++` runs every time, and no error is ever raised.

The plain-statement form (no value collected) already matches raku:

```raku
my $i = 0;
for 1, 2, 3, 4 -> { $i++ };
```

Both `raku` and `mutsu` throw `Too many positionals passed; expected 0
arguments but got 4` here (mutsu's message differs slightly -- "got 4", the
whole list length, vs. raku's "got 1", the first offending element -- but at
least both correctly refuse to run the loop body).

This also affects the `for` **statement modifier** spelling in expression
position, e.g. `say (-> { $i++ } for 1,2,3,4)`, discovered while implementing
the closure-signature-becomes-loop-signature lowering in
`news/2026-08/pointy-block-arity-in-for-statement-modifier.md` -- but the bug
is not specific to the modifier; the plain non-modifier expression-context
form above reproduces it identically, so it predates that change.

## Root cause

`src/compiler/helpers_do_expr.rs`'s `compile_do_for_expr` (the compile path
used when a `for` loop's results are collected as an expression value) never
threads the loop's `explicit_zero_params` flag through to the emitted
`ForLoopSpec` -- it hardcodes:

```rust
explicit_zero_params: false,
```

at (at least) two call sites (lines ~426 and ~528, both marked with a
pre-existing `// TODO: thread params_def through compile_lazy_for_expr` note
covering a related gap). The zero-args guard lives in
`src/vm/vm_for_loop_dispatch.rs`:

```rust
if spec.explicit_zero_params && !items.is_empty() {
    return Err(RuntimeError::new(format!(
        "Too many positionals passed; expected 0 arguments but got {}",
        items.len()
    )));
}
```

which never fires because the spec it receives always says
`explicit_zero_params: false` regardless of what the source actually wrote.

## Why it's a separate ticket

Fixing this means threading a real `explicit_zero_params` (and probably the
full `params_def`, per the existing TODO note already there) through
`compile_do_for_expr`'s several call sites and result-collection paths, which
is more surface than the statement-modifier arity fix that surfaced it. It is
also a pre-existing gap, not a regression from that change.

## Affected files

* `src/compiler/helpers_do_expr.rs` -- `compile_do_for_expr`, the two
  hardcoded `explicit_zero_params: false` sites.
* `src/vm/vm_for_loop_dispatch.rs` -- the zero-args guard that never receives
  a `true` flag from this path.

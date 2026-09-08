# A `NEXT` phaser clobbers the collected value of a value-position `for`

## Root cause

`Compiler::expand_loop_phasers` (`src/compiler/helpers_phasers.rs`) lowers a
`NEXT { ... }` phaser by appending the phaser body to the end of the loop body
(via `rewrite_next_targets_in_stmts`, which also splices it in at every `next`
target). That is correct for statement position, where the loop body's trailing
value is discarded.

In *expression* position it is not: the shared `for` lowering
(`src/compiler/control_for.rs`, `ForParts::collect`) collects the loop body's
last statement value as that iteration's result, and after the phaser expansion
the last statement is the `NEXT` phaser body, not the user's own tail
expression.

## Repro

```raku
my @seen;
my @doubled = do for 1, 2, 3 {
    NEXT { @seen.push: 'next' }
    $_ * 2
};
say @doubled;
```

- `raku`: `[2 4 6]`
- `mutsu`: `[[next] [next next] [next next next]]` — every element is the value
  of `@seen.push`, the NEXT phaser body.

`FIRST` and `LAST` are unaffected (`FIRST` is prepended as a guarded `if`,
`LAST` lands in the post-loop statements), so only `NEXT` — and, by the same
mechanism, the `LEAVE`/`UNDO` splices `rewrite_next_targets_in_stmts` performs —
is at risk.

## Affected files

- `src/compiler/helpers_phasers.rs` — `expand_loop_phasers`,
  `rewrite_next_targets_in_stmts`.
- `src/compiler/control_for.rs` — the shared `for` lowering that collects the
  body's trailing value when `collect` is set.

## Why it is not a one-liner

The phaser expansion is a pure AST-to-AST rewrite with no notion of the position
its result will be compiled in, and the KEEP/UNDO/POST capture logic already in
`expand_loop_phasers` shows the shape of the fix: the user's trailing expression
has to be captured into a temp *before* the phaser bodies run, and re-emitted as
the body's final value afterwards. `expand_loop_phasers` does exactly this for
`Stmt::Take` (the gather-lowered loop-expression form) and for the KEEP/UNDO
`result_var`, so the mechanism exists — but it is currently keyed off which
phasers are present, not off whether the caller wants a value, and every one of
its callers (`while`, `repeat`, C-style `loop`, `for`, both positions) would have
to agree on the new contract. Doing it wrong silently changes what every loop
with a `NEXT`/`LEAVE`/`UNDO` phaser evaluates to.

Note that the statement-position lowering has always been correct here, and the
value-position one has always been wrong — this predates the
statement/expression unification (the old duplicated `compile_do_for_expr`
called `expand_loop_phasers` the same way). Pin for the working cases:
`t/control-construct-value-position.t`.

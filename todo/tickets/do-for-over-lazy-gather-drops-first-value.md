# `do for` over a lazy gather drops the first iteration's value

Minimal repro:

```raku
sub trip($n) { for 1..2 -> \a { take a * $n } }
my @a = do for gather trip(5) { $_ };
say @a;   # mutsu: [10] — raku: [5 10]
```

The statement form (`for gather trip(5) { @got.push($_) }`) iterates both
elements correctly, and plain materialization (`my @a = gather trip(5)`) is
right too (both pinned by `t/gather-take-in-callee.t`); only the expression
form `do for`'s value collection loses the FIRST iteration's block value.

This is pre-existing, not a regression from the C6e-2 lazy-pull fix: before
that fix this shape either crashed ("Interpreter stack underflow in
CallFunc", compiled callee) or produced `[5]` (interpreter-arm callee, also
wrong) — the take-in-callee suspension was unsound in every variant
(`todo/deep/`… see `news/2026-08/gather-take-in-callee-eager.md`). The
first pull now returns the correct element; the do-for collector appears to
discard the value produced during the pull that materialized the list, then
collect normally from the second element on.

Where to look: the do-for (expression) collection path over a `LazyList`
source — the statement/expression asymmetry means the bug is in how the
expression form's accumulator interacts with the first lazy pull, not in the
pull driver itself.

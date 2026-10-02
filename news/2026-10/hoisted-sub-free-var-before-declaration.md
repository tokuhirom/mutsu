# A named sub called before its declaration reads its own lexicals

ADR-0024 made a named sub resolve its free variables against the bindings
visible at its declaration, but left one edge open: a call made textually
*before* the sub's declaration ran while only the hoisted registration had
happened, and that pass found the declaring `my` not yet executed, so it
captured nothing and the call read the caller's same-named variable:

```raku
my $c = "outer";
{ my $c = "inner"; say f() }   # was: inner — now: outer, as in raku
sub f { $c }
```

The compiler now records which of a sub's free variables are declared in
the sub's own scope (`hoist_seed_slots`). For those, the hoisted
registration seeds a fresh container in the declaration's slot and in the
unit store, and the declaration's binding reset and initializer store go
through that container instead of replacing it. A call before the
declaration reads `Any`; afterwards the sub, the declaring scope and the
store share one container, and a loop body binds a fresh one per iteration.
Scalars, arrays and hashes are covered.

The statement-position bare-block hoist used to compile a full second copy of
each sub at the top of the block, where the block's later `my`s were not yet
allocated, so its plan never got the free-variable slot bake. It now emits a
plan-only registration like every other hoist site
(`Compiler::hoist_one_sub_decl`).

Closes mutsu#9911. Test: `t/routines/hoisted-sub-free-var-before-declaration.t`.

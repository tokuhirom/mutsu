# A statement-level `BEGIN {...}` block's side effects on an outer array/hash no longer vanish when a sibling VarDecl triggers phaser reordering

```raku
my $z = BEGIN 99;    # any VarDecl with a nested BEGIN/CHECK/INIT PhaserExpr
                      # is enough to trigger reordering for the whole block
my @order;
BEGIN { @order.push('begin') }
my $i = 20;
@order.push('after');
say @order;          # raku: [begin after]   mutsu (before): [after]
```

`reorder_at_level` (`src/runtime/phasers.rs`) hoists bare declarations ahead
of a statement-level `BEGIN` whenever any `VarDecl` in the same block has a
nested `BEGIN`/`CHECK`/`INIT` `PhaserExpr` (`has_other_phasers`). Two
independent bugs in that hoisting split compounded to silently drop the first
push above:

1. **Wrong "has an initializer" test.** A bare `my @a;`/`my %h;` (no explicit
   initializer) parses with a sigil-based default literal
   (`Literal(Array([]))`), not `Literal(NIL)`. The hoisting split tested the
   initializer expression against a `NIL` literal to decide whether a real
   initializer existed, which wrongly treated that default literal as "has an
   initializer" and spliced a spurious `@a = []` reset into the statement
   list — landing *after* the hoisted `BEGIN` had already mutated the array,
   silently discarding its effect. Fixed by checking the parser's own
   `__has_initializer` custom trait (the same marker several compiler sites
   already rely on for this) instead of guessing from the expression shape.
2. **Wrong interim default for the hoisted declaration itself.** Once (1) is
   fixed, the hoisted bare declaration's own placeholder value used a flat
   `Literal(NIL)` regardless of sigil. Compiling a `NIL` initializer for an
   `@`-sigil variable takes the same path as an explicit `@a = Nil`
   assignment, which itemizes the `Nil` into a one-element `[(Any)]` array
   instead of leaving the array genuinely empty — so `my @a;` under
   reordering was starting from `[(Any)]`, not `[]`. Fixed by giving the
   hoisted declaration a sigil-appropriate empty default (`Array`/`Hash`)
   instead of a bare `Nil` literal.

Root-caused with a temporary env-gated debug print comparing the array's
`Gc` pointer, refcount, and item contents across both pushes: the pointer and
refcount stayed identical throughout (ruling out a container-identity split),
but the item list was reset to empty between the two pushes by a `SetLocal`
that `--dump-bytecode` did not show — `--dump-bytecode` compiles the
*original* (pre-reorder) AST directly, so it never reflects the actual
reordered bytecode a normal run executes. A backtrace on that extra
`SetLocal` pinned it to the spurious rest-bucket `@order = []` assign from
bug (1). Regression test:
`t/statement-level-begin-side-effects-with-vardecl-split.t`.

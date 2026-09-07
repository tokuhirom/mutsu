# `(my %q is SetHash)` used as an expression is a `SetHash` now

```raku
say (my %q is SetHash).^name;      # raku: SetHash   mutsu: Hash    (before)
say (my %q is SetHash) ~~ SetHash; # raku: True      mutsu: False   (before)
```

The same held for `is BagHash`, `is MixHash` and `is Buf`. Declared as its own
statement and read afterwards, the trait was applied and everything already
agreed (`my %q is SetHash; %q.^name` was `SetHash`) — so the container trait
*was* running, just not before the declaration's own value was handed to the
surrounding expression.

## The fix

mutsu applies `is <Type>` through a separate `ApplyVarTrait` op, and the
expression-position `Stmt::VarDecl` path in `src/compiler/expr_block.rs`
special-cased exactly one trait: `is default(...)`, applied *before* reading the
value back so the container's embedded default travelled with the result. Every
other named trait was applied only after the value had been pushed.

Two changes, both generalising that existing precedent:

- The `@`/`%` branch applies every **argument-less** named trait alongside
  `default`, before its `GetArrayVar`/`GetHashVar` read-back. Traits that take
  an argument are left to the statement path — their argument would have to be
  compiled onto the stack ahead of the value this expression is still building.
- The general branch's post-trait read-back, previously gated on
  `has_default_trait && is_nil_init` for scalars only, now also fires for an
  `@`/`%` declaration carrying any named trait, since a container trait
  *replaces* the container rather than annotating it.

## Measured against `raku`, all matching

`(my %q is SetHash)`, `is BagHash`, `is MixHash`, `(my @a is Buf)`; the
smartmatch `(my %r is SetHash) ~~ SetHash`; the declaration as a `given` topic
and as an assignment RHS; the unchanged statement form, including
`%w ~~ Set` staying `False`; and `is default(...)` on both sigils, which still
travels with the value.

## Split off, deliberately

One row of the ticket's neighbourhood list turned out to be a *different* bug
and is filed as `todo/tickets/do-block-tail-declaration-drops-its-trait.md`: a
declaration that is a `do` block's tail statement
(`my $z = do { my %u is SetHash }`) emits **no `ApplyVarTrait` at all`**, and
loses `is default(...)` the same way for both sigils — so it is not the
"expression returns the pre-trait value" shape this fix addresses, and it takes
a third compile route that never looks at `custom_traits`.

## Testing

New `t/inline-container-trait-expression.t` (11 assertions), which passes
unchanged under rakudo.

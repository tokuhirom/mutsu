# RakuAST: routine return types and hyper infix operators (and a dropped pointy return type)

Two of the read-direction representation gaps listed in
`todo/deep/rakuast-remaining.md` are closed, in both directions (`.AST` read and
`EVAL` lowering), and pinned by dual-oracle tests that pass verbatim under both
mutsu and the system raku.

## Signature return types

raku models the two spellings of a routine return type with different nodes:

- `sub f(--> Int)` puts it in the signature — `Signature.returns => Type::Simple`
- `sub f() returns Int` makes it a routine trait — `Trait::Returns`
- `sub f() of Int` makes it a *different* routine trait — `Trait::Of`

mutsu's internal AST already distinguished the first two (the parser records a
`__return_via_trait` marker in `custom_traits` for the trait spelling, because an
undeclared `returns` type is `X::InvalidType` while an undeclared `-->` type is
`X::Undeclared`). It collapsed `of` into the same marker, so the parser now sets a
distinct `__return_via_of` for it — a small, honest internal-AST refinement rather
than a guess in the converter. All three node choices now come straight out of the
parse; `EVAL` lowers them back to `SubDecl.return_type` plus the matching marker,
so a round-trip reproduces the source spelling.

Rendering a parameter-less signature exposed a small renderer bug: raku prints an
empty attribute list as the itemized `$( )`, while mutsu printed a bare multi-line
`(\n)`. `RakuAST::Signature.new.gist` now matches Rakudo exactly.

## A pointy block silently dropped its return type

Extending the same work to pointy blocks surfaced a real, RakuAST-independent
correctness bug. A *single*-parameter pointy block parses to the internal
`Expr::Lambda` node, which has no field for a return type — so the parser parsed
`--> Int` and threw it away:

```raku
my $f = -> $x --> Int { "s" };
say $f(1);      # mutsu printed "s"; raku dies with a return type check failure
```

The multi-parameter form (`-> $a, $b --> Int`) was fine, because it uses
`Expr::AnonSubParams`, which carries `return_type`. The fix routes a pointy block
that declares a return type through that same node, so the constraint survives to
the compiler. Pinned by `t/pointy-block-return-type.t`.

## …which in turn exposed a second one: an inner block inherited the check

Enforcing the pointy-block return type turned the battery gate red on
`Text::CSV`'s `90_csv.t`, and the cause was a *separate*, pre-existing bug that
the new enforcement merely made reachable. The closure-construction path for a
**bare block** kept a lexically captured `__mutsu_return_type`, so a block written
inside a routine that declares one enforced the outer routine's type on its own
inner value:

```raku
-> $x --> Pair { (@k.map({ $x{$_} }).join: ":") => $x }
# Type check failed for return value; expected Pair but got Str ("1")
```

The `{ $x{$_} }` argument to `.map` was being checked against `Pair`. The
`MakeLambda` and `MakeAnonSubParams` arms already dropped the inherited marker;
`MakeAnonSub` (the bare-block arm) did not. It does now. The bug was reproducible
without any of this slice's other changes (a two-parameter pointy block hits it
too), so it had been latent since the marker was introduced. Pinned by
`t/return-type-not-inherited-by-inner-block.t`.

## Hyper infix operators

`@a >>+<< @b` now renders as
`ApplyInfix(left, MetaInfix::Hyper([dwim-left,] infix, [dwim-right]), right)` and
lowers back through `EVAL`. mutsu's `Expr::HyperOp` already kept the operator text
and both dwim flags, so this needed no parser change at all — every `<<`/`>>`
combination (`>>+<<`, `<<+>>`, `>>+>>`, `<<+<<`) matches Rakudo's gist byte for
byte, including raku's habit of omitting a dwim field whose value is False.

The remaining hyper boundary is narrower now: hyper prefix (`-<<@a`), hyper
postcircumfix (`@a>>[1]`), hyper function infix (`>>[&f]<<`), and `@a<<.abs`
(which mutsu's parser currently reads as a quote-words subscript).

## Tests

`t/rakuast-return-type.t` (21 assertions), `t/rakuast-hyper-infix.t` (15),
`t/pointy-block-return-type.t` (8), and
`t/return-type-not-inherited-by-inner-block.t` (6). See
[ADR-0011](../../docs/adr/0011-rakuast-model-layer-and-phasing.md) for the updated
divergence record and `todo/deep/rakuast-remaining.md` for what is still open.

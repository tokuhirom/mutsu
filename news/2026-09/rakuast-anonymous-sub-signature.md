# RakuAST anonymous subs with an explicit signature

`sub ($x) { }` now renders as a nameless `RakuAST::Sub` instead of a
`RakuAST::PointyBlock`, and a nameless `RakuAST::Sub` lowers back through
`EVAL`. This closes the "anonymous subs with explicit signatures" read-direction
gap in `todo/deep/rakuast-remaining.md`.

## Root cause

mutsu parses `sub ($x) { }` and `-> $a, $b { }` to the same internal node,
`Expr::AnonSubParams`, which recorded nothing about which declarator the source
wrote. raku models the two with different nodes, and the difference reaches the
signature as well: a sub/method parameter carries the implicit
`type => RakuAST::Type::Setting(Any)` that a pointy block's parameter does not.
With the distinction erased before conversion, the converter had no honest
choice and rendered every parameterised anonymous closure as a `PointyBlock` —
a divergence `t/rakuast-anon-sub.t` documented in prose rather than pinned.

## Change

`Expr::AnonSubParams` gained an `is_sub` flag, set only where the parser reads
the `sub` declarator (`parse_anon_sub_with_params`). Everything else that lands
on this node keeps it false: pointy blocks, placeholder blocks, `method (...)`
literals, and the closures the compiler and runtime synthesize. The flag has no
execution meaning — both spellings compile to identical bytecode, and the
compiler's `AnonSubParams` arm ignores it explicitly.

- `src/rakuast/convert.rs` renders an `is_sub` closure through a new
  `anon_routine_node`, the same shape as `routine_node` minus the `name` field,
  so its parameters carry `Type::Setting(Any)`.
- `src/rakuast/lower.rs` splits `RakuAST::Sub` by whether it has a `name`: a
  named one is still a declaration (`lower_sub`), a nameless one is now a
  closure *value* in expression position, lowering to `Expr::AnonSub` when it
  has no signature and `Expr::AnonSubParams { is_sub: true }` when it does. A
  nameless sub carrying a `returns`/`of` trait is refused rather than silently
  dropped, because the internal node keeps no `custom_traits` to round-trip it.

## Side effect: `EVAL` of a parameter-less anonymous sub

`EVAL(Q[my $f = sub { 42 }; $f()].AST)` used to fail with
`EVAL does not yet support lowering RakuAST::Sub` — the read direction produced
a nameless `RakuAST::Sub` that the write direction could not consume, because
`RakuAST::Sub` was only ever dispatched as a *declaration*. Adding the
expression-position arm closes that round trip too.

## Coverage

`t/rakuast-anon-sub-signature.t` (17 assertions) pins the new `Sub` shape and
its `Type::Setting(Any)` parameters, that pointy blocks (single- and
multi-parameter) and bare blocks are unchanged, that a parameter-less anonymous
sub still omits its signature, `-->` in an anonymous signature, the accessors,
and all three `EVAL` round trips. It is a dual-oracle test: it passes verbatim
under both mutsu and raku. The prose divergence notes in `t/rakuast-anon-sub.t`
and `t/rakuast-eval-anonsub.t` were updated to match.

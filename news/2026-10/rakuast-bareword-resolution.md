# RakuAST: barewords resolve the way rakudo's parse-time lookup does

A bareword was the most common first refusal of the RakuAST round trip
(`MUTSU_RAKUAST=1`): 740 of the 4008 `t/` files outside the ratchet stopped
on one. Rakudo resolves a bareword when it parses it, so its RakuAST node
says what the name is. mutsu's parser leaves every one as `Expr::BareWord`,
and the converter now re-derives the node rakudo picks (measured on 2026.09):

- **The unit's names inside a block.** Each block body collected its own
  declared names and *replaced* the unit's set with them. So in
  `class C { }; (1, 2).map({ C.new })` the closure could no longer see `C`.
  The unit-level scan already enters every block, so a block body now keeps
  that set. This fix alone moved 127 files.
- **Setting terms.** A setting enum value (`Less`, `Kept`,
  `SeekFromBeginning`) renders as `Term::Enum`. Any other defined setting
  symbol (`IterationEnd`, `Order::Less`, `SIGINT`) renders as `Term::Name`.
  The list is generated from rakudo's own `.AST` by
  `scripts/gen-core-term-names.raku` (`src/rakuast/core_term_names.txt`), the
  companion of the type-name list.
- **The unit's enum values and sigilless parameters.** `Red`, `Color::Red`
  and the `x` of `sub f(\x) { x }` or `-> \v { v }` render as `Term::Name`.
  The parameter itself now renders as `ParameterTarget::Term`. Before, it
  became a `ParameterTarget::Var("$x")`, which lowered back to an ordinary
  scalar parameter.
- **Sigilless slurpies.** `+a` and the capture `|c` (or the anonymous `|`)
  render as `Slurpy::SingleArgument` / `Slurpy::Capture` with a term target.
  Before, `|c` rendered as a flattening `*$c` and lowered back to one.
- **Redispatch without arguments.** `callsame`, `nextsame`, `lastcall` and
  `nextcallee` render as `Call::Name::WithoutParentheses`.

The bareword logic moved out of `convert.rs` into `src/rakuast/bareword.rs`.

Once files ran further, a survey of the newly reached ones found several
silent miscompiles in lowering:

- `op_name_to_token_kind` was missing the inverse of many
  `token_kind_to_op_name` rows. `..^`, `^..` and `...^` lowered to an
  undeclared named infix and died "Two terms in a row". `and` / `or` lowered
  to an eager call, so `5 < 3 and say "X"` printed `X`. A unit test now
  checks the round trip for every infix token.
- A pointy block whose single parameter destructures (`-> [$a, $b] { … }`)
  collapsed to an `Expr::Lambda`, which has no field for a sub-signature.
  The inner names were then left unbound.
- A typed `my Foo $u .= new(...)` did not mark its call as an initializer.
  The converter therefore rendered a bare `my Foo $u` and dropped the call.

The round-trip ratchet grows from 1910 to 2438 of 5968 `t/` files. Pinned by
`t/rakuast/rakuast-bareword-resolution.t`, which passes under both
mutsu and raku.

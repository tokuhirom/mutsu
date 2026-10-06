# RakuAST: members, signatures and declarations

The traits and signatures of attributes, methods, subs and parameters now read
back as the nodes rakudo has (measured on rakudo 2026.09), and the lowering
rebuilds the parser's own expansion, so the round trip is the parsed program.

- **Attributes.** `has $.x is rw is required` keeps its traits in written order
  (`HasDecl` now records the kinds in `trait_order`, `ast::attr_trait`):
  `is required("why")`, `is DEPRECATED`, custom `is marked(5)`, `is TYPE` on an
  `@` / `%` attribute, `does ROLE` and `handles` render as `Trait::*` nodes in
  that order. A `where`, the `our` / `my` scope and the alias `has $x` (no
  twigil) are rendered too.
- **Methods.** `my` / `our` scope, custom traits (as on a sub), `is default` and
  `is DEPRECATED`. A sub's `is DEPRECATED` is a `Trait::Is` as well.
- **Operator subs.** `is assoc<left>` and `is tighter` / `looser` / `equiv(&op)`
  render when they are the sub's only trait. The parser derives a `__prec`
  record from them; `parser::op_prec_trait` is the one builder, so a lowered
  operator sub carries the same record.
- **Parameters.** `+@a` / `+$a` / `+%a` (`Slurpy::SingleArgument`), literal
  values (`sub f(1)`, `-> "a" { }`: no target, the literal's type, the value),
  custom traits with arguments, `$x? = 3`, a `where` on a slurpy, a sigilless
  invocant (`method m(\S:)`).
- **Declarator lists.** `my (Int $a, \b, *@r, $c is rw) = ...`: typed,
  sigilless, slurpy and `is rw` / `raw` / `copy` / `readonly` elements; the
  declaration's own type is the signature's `returns`; a `:=` list has no
  `default-rw`.
- **Anonymous routines.** `sub () is rw { }` / `method () is rw { }`, and a
  method literal's declared invocant (`method (Foo:D $x: $y)`), which the
  parser folds into the receiver's type and a `my $x := self` binding:
  `parser::folded_invocant` reads that fold back and
  `parser::anon_method_expr_declared` builds it again.
- **Declarations.** `our Mu constant X = 1`, `my Int $n where * > 0 = 3`, a
  role's own traits (`role R[::T] is array_type(T)`), and the `scope`,
  `twigil` and `where` accessors of `VarDeclaration::Simple` answer rakudo's
  defaults.

Left for later in the plan (not S4): the `where * > 0` and `where { ... }`
spellings differ from rakudo's (`Term::Whatever`, `may-have-signature`) and so
do operator sub names (`Name.from-identifier("infix", colonpairs => ...)`) — S9;
using a user-defined operator (`1 +++ 2`) is an `InfixFunc` node — S6; a code
signature `&c:(Int --> Str)` and a shaped array parameter are dropped by
rakudo's own tree, so they stay refused.

New tests: `t/rakuast/rakuast-members-traits.t` and
`t/rakuast/rakuast-signature-members.t`, whose tree parts also run under
`raku`. Slice S4 of #7564.

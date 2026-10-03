# RakuAST: parameterized roles, role header traits and `also does`

A re-survey after the regex-atom slices found 133 `t/` files that stopped at
`.AST` on a role. In 89 of them the role was parameterized; in 60 its header
carried a `does` or `is` clause. Measured on rakudo 2026.09:

- `role R[::T]` / `role R[$a, Int :$b = 2]` is a `Role` with a
  `parameterization` field. That field is a `Signature` whose parameters carry
  no implicit `Type::Setting`, unlike a routine's.
- The header's clauses are the role's `traits`: `does S` and `does S[Int]` are
  `Trait::Does` (with a `Type::Parameterized` for the second), `is P` is
  `Trait::Is`, and `is export` / `is rw` read and render through the same
  `IsTraits` code routines use.
- `also does S;` in a role or class body is `Statement::Also`.

The parser folds a header `does S` and a body `also does S` into the same
`DoesDecl` statement. Following ADR-10723's rule for a spelling the parser
normalizes, the statement now records which form it was (`also`), instead of
the converter guessing from its position. The names a role substitutes for
its parameters come from one helper, `role_type_param_names`, shared by the
parser and the lowering. `Trait::Does` also exposes `.type`.

Rakudo lists a role's traits in source order. The parser does not keep that
order across the parent clauses, `is export` and `is rw`, so a role that mixes
them still declines rather than rendering an invented order.

The round-trip ratchet grew by 74 files, to 3099.

The work turned up three bugs outside the slice, filed as issues:
- a punned `role Pun is P` does not inherit from `P` (#11623);
- `Trait::Does.new` is not a known constructor, and
  `Name.from-identifier(...).parts` is empty (#11624);
- a parameterized type with a named argument, such as `does S[:y(2)]`, is
  refused (#11625).

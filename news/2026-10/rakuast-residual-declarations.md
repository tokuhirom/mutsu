# RakuAST: anonymous, typed-term and user-operator declarations

Three more declaration shapes round-trip under `MUTSU_RAKUAST=1`:

- `my $ = 5 andthen ...`: the anonymous declaration leads the loose tail, so the
  application's first operand is `VarDeclaration::Anonymous` (the parser's
  re-read of the anonymous variable is swapped for it).
- `my Int \x .= new`: the typed sigilless call-assign declaration keeps its
  type, and the lowered call's invocant is the declared type rather than `Mu`
  (the ordinary frontend already computed `0`; the round trip printed `Mu.new`).
- `my $x foo= 6` with a user-declared `infix:<foo>`: the declaration is the left
  operand of an `ApplyInfix` over `MetaInfix::Assign`, and lowering rebuilds the
  parser's `InfixFunc` expansion.

`roast/S03-operators/andthen.t`, `inplace.t` and
`roast/S06-operator-overloading/infix.t` pass under the RakuAST frontend.

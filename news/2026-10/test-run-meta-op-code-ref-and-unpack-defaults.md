# `&[R[~~]]` and the defaults of an omitted unpacking named parameter

Two gaps found with the Test::Run distribution:

- `&[R[~~]]` (and `&[Z[+]]`, `&[X[+]]`) named a routine `infix:<R[~~]>` that failed with "Two terms in a
  row" when called. The brackets only group the inner operator, so the reference now spells it `R~~`.
- A named parameter that unpacks its argument (`:exitcode([$e = 0, :pass($p) = True])`) left `$e` undefined
  when `:exitcode` was not passed, because the omitted-argument placeholder unpacked as one undefined
  element. It now unpacks an empty list, so the inner defaults apply.

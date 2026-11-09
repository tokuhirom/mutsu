# A role method's `--> ::?CLASS` return type works on a pun

`method a(--> ::?CLASS) { self }` called on `R.new` answered `Nil`: the
return-spec check took the pseudo-type for a definite value expression. It is a
type, and on a pun (the role itself) `::?CLASS` / `::?ROLE` mean the role, so a
wrong value now fails with `expected R but got Int (5)` as in rakudo (#17238).

# `.origin` of regex internal modifiers

A regex's internal modifier ended the origin walk of its body, so it left the
modifier and every atom after it without a range. This covered `:i`,
`:!ignorecase`, `:s`, `:m` and `:r`, in `/ :i a /` and `token w { :i $x }`.
The walk now reads the modifier as written: the colon, a `!` exactly when the
node is negated, and the short or long name. It then goes on with the atoms
after it, as rakudo 2026.11 places them (#15780).

# RakuAST: origin of `$.n` on the left of a compound assignment

`$.n += 100` as a statement now gives its `Var::Attribute::Public` the range
rakudo does, as `$x += 1` and `$!n += 1` already did (#15780).

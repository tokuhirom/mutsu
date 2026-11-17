# `sort` ignores a `:by` named argument, as rakudo does

`(3, 1, 2).sort(:by(-*))` and `sort(:by(-*), 3, 1, 2)` used `:by` as the comparator and answered
`(3 2 1)`. No `sort` candidate declares `:by` -- the method's and the sub's `*%_` swallow it -- so
rakudo sorts in the default order, `(1 2 3)`; mutsu now does too. `min`/`max`/`minmax`, which do
declare `:&by`, keep it. Closes #17844.

# Multi dispatch treats where-vs-nominal splits as incomparable

Multi candidates are now also compared parameter-wise. When one candidate wins a
parameter through a `where`/`subset`/literal refinement and the other wins a
different parameter through a nominal type, they are incomparable, as in Rakudo,
and the first declared candidate that binds wins instead of the summed
narrowness key. This fixes `CSS::Module`'s `parse-property` picking the
coercion candidate over the `where` one (#11943).

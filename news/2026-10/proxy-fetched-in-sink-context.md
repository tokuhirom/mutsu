# A Proxy is FETCHed when its value is sunk

A statement whose value is a `Proxy` (a block's or an `is rw` accessor's result
that nobody keeps) now runs the Proxy's FETCH, as Rakudo does when it sinks the
container. `dies-ok { $array.required-field }` therefore sees the FETCH die;
the block used to hand the Proxy back untouched. PDF's `t/cos-tie-index.t`
required-field assertion was the case.

# `is copy` parameters detach sigilless binding metadata

A scalar `is copy` parameter now clears inherited sigilless alias and
readonly metadata before it binds its own detached value. Previously, an
assignment to an `is copy` parameter with the same name as an outer sigilless
capture could follow that capture back to an immutable literal.

The regression was exposed by the vendored upstream `Test` module while running
`roast/S32-num/rat.t`. The generic call-chain shape is pinned by
`t/is-copy-sigilless-capture-chain.t`.

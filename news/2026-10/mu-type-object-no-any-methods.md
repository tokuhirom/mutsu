# The Mu type object does not answer Any's methods

`Mu.List`, `Mu.list`, `Mu.Array`, `Mu.first`, `Mu.elems`, `Mu.sum` and `Mu.join` are now
`X::Method::NotFound`, as in Rakudo: `Mu` is not an `Any`. One gate (`cool_method_not_found`,
consulted by both the VM and the interpreter entry) refuses every name `Any` declares and `Mu` does
not, instead of a per-method check (#12976).

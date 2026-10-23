# `.^isa` of a concrete container follows its `.^mro`

`$x.VAR.^isa(Any)` and `.^isa(Mu)` answered 0: a concrete `Scalar` has no class entry whose MRO the
check could walk, so only the exact name matched. `.^isa` now falls back to the chain `.^mro`
reports for the receiver. Found with AttrX::Mooish `t/010-base`'s unbinding subtest, whose remaining
gap is that `Attribute.get_value` answers the decontainerized value (`$attr.get_value($obj).VAR` is
the value's `VAR`, not the attribute's `Scalar`/`Proxy`).

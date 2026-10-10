# Set operators keep the role mixin on a scalar operand

`set() (|) $x` (and every other set operator, plus `%h (|)= $x` on an object hash) stripped any
role mixin from a non-aggregate operand, so an `Attribute` that had been given a role with
`$attr does R` came back as a bare `Attribute` with none of the role's methods. Red's
`%!relationships ∪= $attr` hit this and `$rel.build-relationship` died with "No such method".
A role mixin is now stripped only when what it wraps is an aggregate the operator flattens
anyway (hash, list, set/bag/mix, Baggy subclass); a role-mixed scalar stays one whole element.

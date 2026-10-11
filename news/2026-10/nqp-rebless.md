# nqp::rebless

`nqp::rebless($obj, $type)` now gives an object a mixin type in place (#11499). It composes the
roles of `$type` (a `$obj.^mixin(R)`-style mixin type object) onto `$obj` through the same
mutating composition `.^mixin` and `does` use on a real object, so every alias sees the new type,
and answers the object. A type that is not a mixin type dies with Rakudo's "New type X for Y is not
a mixin type"; reblessing to the object's own type is a no-op.

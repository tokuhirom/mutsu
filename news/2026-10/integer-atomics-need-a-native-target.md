# Integer atomics refuse a target that is not a native-integer container

`my $x = 1; $x⚛++`, `$x ⚛+= 2`, `atomic-fetch-add($x, 2)` and
`nqp::atomicinc_i($x)` answered a value and bumped the variable. Rakudo's only
candidate is `atomicint $target is rw`, so on a plain `my $x` (or `my Int $x`,
`my uint $x`, a plain array or hash element, a plain or `Int` attribute, a
read-only parameter) it is a dispatch failure: `X::Multi::NoMatch`, "Cannot
resolve caller postfix:<⚛++>(Int:D); the following candidates match the type
but require mutable arguments", naming the operator as it was written and
listing the candidate signatures. The `nqp::` `_i` ops raise MoarVM's own
`X::AdHoc` ("Can only do integer atomic operations on a container referencing a
native integer"). `int`, `atomicint` and `int64` variables and attributes,
native-int array elements and `is rw` parameters bound to them work as before,
and `atomic-fetch` / `atomic-assign` / `cas` / `⚛=` stay legal on any scalar
(#11834).

The compiler decides it from the declaration. A scope frame now records the
declared type of the scalars it declares, and a nested sub or closure inherits
the frames, so a module-scope `my atomicint $hits` is seen from an exported
routine running on a worker thread -- the case a name-keyed run-time check got
wrong. A declared native target compiles with no guard, so a hot `$n⚛++` loop
is unchanged; anything a declaration does not decide (an attribute, an element,
a parameter, an alias) asks its container at run time and is refused only when
the container is positively known not to be native. See ADR-11834.

Not covered, filed separately: an untyped variable passed to an `is rw`
parameter (#12007), the narrow-int refusal (#12008), a shadowing inner-block
`atomicint` (#12006) and `@a[0] ⚛+= n` (#12005).

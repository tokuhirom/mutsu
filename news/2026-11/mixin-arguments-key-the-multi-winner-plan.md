# A role mixin over a class keys the multi-dispatch winner plan

Reading `$c[$i]` on a `CArray[num64]` ran the imported 6-candidate
`postcircumfix:<[ ]>` family through the full by-value ranking walk on every
read: building a `FunctionDef` per candidate, fingerprinting, ranking all six
against `(array, index)`. The plan cache that already answers a keyable
multi call from a stage program (`captured_multi_plan_winner`, #16172) never
applied, because `multi_arg_type_keys_impl` refused to key any `Mixin`
argument, and a typed `CArray` is a role mixed into the `CArray` class.

A mixin over a class instance or type object now keys as one symbol naming
its base class and role composition (`mixin_composition_key`, the identity
`.WHAT` already uses), memoized per composition node
(`Interpreter::mixin_composition_key_sym`; the entry keeps the node alive so
its address cannot be reused). A mixin over any other inner value (an
allomorph, a native value) stays unkeyed, so its dispatch is unchanged.

Callgrind slope on a profiling build (second run, N 500 -> 1500 reads):
`$x = $c[$_ % 500]` on a `CArray[num64]` 850.9k -> **151.4k Ir per read
(-82%)**. The rest is the winner's own call: the generic closure bind of
`CArray:D \array, $pos` (~41k) and its body (~43k).

Refs #16373.

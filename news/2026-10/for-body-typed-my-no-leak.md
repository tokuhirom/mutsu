# A typed `my` in a `for` body no longer leaks past the loop

A `my Str $nm` in a `for` body that also declared an `@`-array from the topic
stayed readable by its bare name after the loop (and from other modules, which
broke CSS::Properties' sigilless parameter defaults, #13516). The block-entry
type hoist seeded the type object for the unbound name, so the declaration's
own `SetVarDynamic` saw an existing binding and recorded no removal marker.
The hoist now records the name's absence for the body scope, whose exit takes
the seed away again.

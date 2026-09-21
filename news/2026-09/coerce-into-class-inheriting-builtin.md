# Coercing into a class that inherits a built-in type

`T(value)` where `T` is a class inheriting a built-in scalar type
(`class CM is Str {}; CM('x')`) used to die with
`X::Coerce::Impossible: Impossible coercion from 'Str' into 'CM'`, because
mutsu's coercion protocol only tried a *user-declared* `COERCE`/`new` on the
target class. Rakudo answers this through the built-in parent's *inherited*
`COERCE`, building a real `CM` that carries the string — the class had
nothing to reach that with, since `Str.COERCE` did not exist in mutsu at all,
and even a hand-rolled `new` had no slot to hold the payload in (a plain
`Instance` has none).

The sibling case — a *role* that inherits a built-in type
(`role Markup is Str {}`) — was already fixed: a non-`Instance` value has no
shared attribute node to rebless, so the role rides in mutsu's mixin wrapper
around the coerced built-in value. A class instance is different: it already
*is* an `Instance` with a shared `InstanceAttrs` node, so no wrapper is
needed. `types::native_backed_class` boxes the coerced built-in payload into
a private instance attribute (`__mutsu_native_backing`, never user-visible)
instead, covering the same set of built-in scalar parents the role case does
(`Str`, `Int`, `Num`, `Rat`, `FatRat`, `Complex`, `Bool`) — container
built-ins (`is Array`/`is Hash`) already have their own backing-storage
mechanism and are unaffected.

This lands the coercion-protocol entry points: the `T(value)` call syntax,
the `T()` type-constraint spelling (`sub f(CM() $x)`, `--> CM()`), and the
explicit `.COERCE(value)` method call — plus `.^lookup('COERCE')` correctly
reporting the method as inherited from the built-in parent rather than
absent. A boxed instance's `.Str`/`.gist`/`.raku`/... and any other method
not overridden by the class itself delegate to the boxed payload, the scalar
twin of the existing `is Array`/`is List` subclass backing-storage
delegation.

General native-method delegation for such an instance beyond what the
coercion protocol needs was already covered by the delegation added here,
reusing the array-subclass delegation's own dispatch entry points
(`call_method_with_values_inner` and the `CallMethod` opcode path).

Closes #8856.

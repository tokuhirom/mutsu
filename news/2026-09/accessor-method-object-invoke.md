# Invoking an attribute accessor's Method object runs the accessor

`D.^lookup("x")($d)`, `D.^find_method("x")($d)`, `D.^can("x")[0]($d)` and
`$d.can("x")[0]($d)` all answered `Nil` for an auto-generated attribute
accessor (`has $.x`): the Method object those routes built carried an
empty-bodied `Sub` as its callable. Only the object `.^methods` hands out
worked. Every accessor Method object — instance attribute, `is rw` attribute
and class-level `my $.x` — is now built by one constructor on the same native
`Routine` carrier, so invoking any of them reads the attribute, and a `.wrap`
on the object still applies.

Assigning through such an object (`D.^can("y")[0]($d) = 5`, and likewise
through a declared `is rw` method's object, `D.^lookup("z")($d) = 9`) now
writes through: the callable-lvalue path treats a Method object called with
its invocant as the spelling of `$d.$m() = 5` and takes the method-lvalue
route. Along the way, an `is rw` method whose body is a bare `$!attr` committed
its store only when the invocant was a named variable, so `f().z = 9` silently
dropped the write; the store now commits into the instance's shared attribute
cell unconditionally.

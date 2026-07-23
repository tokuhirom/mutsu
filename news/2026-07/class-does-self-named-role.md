# A class may compose a CORE role of its own name (`class Iterator does Iterator`)

A class whose name collides with the name of a CORE role it composes — most
notably `class Iterator does Iterator`, a common idiom for a bespoke iterator —
was rejected. At the top level it raised
`X::Inheritance::SelfInherit: class 'Iterator' cannot inherit from itself`, and
inside a package (`unit package Tree::Binary; class Iterator does Iterator`) it
produced a false `C3 MRO cycle detected at Tree::Binary::Iterator`. Rakudo
accepts both: a class cannot compose *itself*, so `does Iterator` resolves to the
like-named CORE `Iterator` role.

The root cause was name resolution treating the `does`-role parent as the class
under declaration. Inside a package, the bare parent `Iterator` was
package-qualified to the class's own fully-qualified name
(`Tree::Binary::Iterator`) as a "sibling" class, so the role landed in the
class's own C3 inheritance-parent list and the class became its own ancestor. At
the top level the bare `Iterator` simply equalled the class name and tripped the
self-inheritance guard.

`register_class_decl` now recognizes a `does`-parent whose *short* name equals the
class's own short name and which names a registered role: it is composed as that
role (the CORE `Iterator`) and excluded from the C3 inheritance parents, so it is
neither self-inheritance nor a self-cycle. Genuine self-inheritance via `is`
(`class A is A`) and a `does` on a same-named non-role (`class B does B`, no such
role) still error, matching Rakudo.

This unblocks the `Tree::Binary` distribution's module load
(`01-basic.rakutest` passes); its iterator tests progress to a separate,
deeper blocker in parametric-role requirement satisfaction. Pin:
`t/class-does-self-named-role.t`.

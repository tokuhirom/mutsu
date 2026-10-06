# `.WHAT` of an anonymous class keeps the class's identity and name

Two textually distinct anonymous classes are two types, and `$a.WHAT` is the
class itself named `<anon|N>`. mutsu answered a nameless type object for both,
so `$a.WHAT === $b.WHAT` and `$a.WHAT eqv $b.WHAT` were `True`, both shared one
`.WHICH` (`|U1075`), `.WHAT.^name` was empty and `.WHAT.gist` was `()`
([#12018](https://github.com/tokuhirom/mutsu/issues/12018)).

`.WHAT` (and the type-object `gist`) blanked every internal `__ANON_*__`
marker, which is right only for an anonymous enum (its type object is the
empty `()`). An anonymous `class`/`grammar`/`role` is a named type: the display
code already rendered its marker as `<anon|N>` (`anon_type_display_name`), but
`.WHAT` dropped the marker before it got there. `is_nameless_anon_type_name`
now says which markers have no name — a marker is nameless only if it is an
`__ANON_*__` one that has no `<anon|N>` display — and `.WHAT` for a package, an
instance and the final fall-back, plus the three type-object `gist` sites, ask
it. `$a.WHAT` is therefore `$a` and an instance's `.WHAT` is its class, whose
identity and name are the ones the registry already kept apart.

Pinned by `t/oo/class/anon-class-what-identity.t`, whose expectations were
taken from `raku` (distinct `===`/`eqv`/`.WHICH`, `.WHAT === $a`, an
instance's `.WHAT`, `.^name`/`.gist`/`.raku`, an anonymous grammar and role,
and the named-class and builtin controls).

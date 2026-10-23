# AttrX::Mooish: the superseded-HOW variants run, and only `t/090` and one alias test remain

A role mixed into the metaobject of a class declared under an `EXPORTHOW::SUPERSEDE`
HOW (a user-class instance, so the mixin reblesses it) now has its
`create_BUILDPLAN`/`publish_method_cache` called as the class composes, and sees the
`@!BUILDPLAN` of the class it describes. The same HOW is installed before a class's
roles compose, so a parameterized role's attribute trait mixes into the HOW that stays;
a hoisted pre-pass no longer loses those mixins. Also fixed on the way: `.VAR` of an
attribute container answers the `Scalar` metamethods (`$attr.get_value($o).VAR.^isa(Scalar)`),
`Metamodel::ClassHOW.HOW` is an `NQPClassHOW`, `Type but R` specializes `R` onto a native
type object (so a role HOW can refuse it), `nqp::clone_nd` of a typed container keeps its
Nil-reset default, `$how.archetypes` takes no type, and `.^candidates[0].HOW` of a role
declared under a superseding role HOW is that HOW.

The AttrX::Mooish 1.0.10 suite now passes in both variants except `t/080-aliasing`
(`but/does`: assigning through the alias of a lazy `rw` mixin attribute) and
`t/090-concurrent` (200k Mooish instantiations; a performance question).

# A method with omitted optional named parameters keeps the fast binder

A method or submethod whose optional named parameters were left out of the
call (`method m(:$a, :$b)` called as `$o.m`, or the `submethod TWEAK(:$authority,
:$host)` every `.new` runs) fell to the general binder on every call. The fast
binder's named-parameter plan only admitted an omitted parameter that had a
constant default, so a parameter with no default at all, which simply binds its
type object, kept the whole call on the slow path, and kept it off the
method-call plan too.

The plan now admits an omitted parameter with no default when it is untyped or
typed with a plain built-in type: the fast binder binds the same type object
the general binder would. `UInt` (whose omission binds its refinee `Int`), a
`:D`/`:U` smiley and any user type still take the general binder.

A call of `method m2(:$a, :$b)` with neither argument went from ~26.0k to
~13.8k instructions, against ~10.2k for a method with no parameters, and a
`.new` on a class whose `TWEAK` takes two such parameters from ~44.1k to
~29.6k.

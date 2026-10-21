# `obj but R` composes a real mixin class and runs its HOW hooks

`$obj but R` on an instance of a user class used to wrap the value in a
mixin map, so a role whose HOW overrides `specialize` (AttrX::Mooish's
aliased, lazy attributes) never specialized onto a new type. It now reblesses
a copy of the object into the synthesized `C+{R}` class, as Rakudo does, and
composes that class's HOW (`compose`, `create_BUILDPLAN` overrides) and runs
its replaced BUILDPLAN. A lexical class's mixin type also keeps its `+{R}`
suffix in `.^name`. AttrX::Mooish `t/080-aliasing` now passes.

# A replaced BUILDPLAN keeps `is default(...)` values

A class whose BUILDPLAN a metaclass mixin replaced (AttrX::Mooish does this for
every class that uses `is mooish`) lost the `is default(...)` value of its
attributes: `has $.initial is default(pi)` read `Any`. Rakudo stores the default
on the attribute's container at allocation, so no plan task carries it. The
plan constructor now seeds each `is default` attribute of the class's MRO before
running the tasks. AttrX::Mooish's `t/010-base` now initializes lazily-built
attributes from the builder's `$!initial` (#14266).

# User trait arguments on classes, roles and packages run in the declaration phase

A class, role, module or package with a user `trait_mod:<is>` trait is now a
BEGIN-time effect of the unit prologue (ADR-0134): its declaration moves into
the prologue, so the trait callback runs once, in source order with `BEGIN`, and
its argument sees the unit's lexicals in their static state. `class C is
paint($shade) {}` after `my $shade = 7` passes `Any`, as Rakudo does, instead of
the initializer's `7` (#14146).

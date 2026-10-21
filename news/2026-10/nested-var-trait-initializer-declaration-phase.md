# A user trait on a nested variable with an initializer runs in the declaration phase

`my $x is tag(1) = 5;` inside a block or sub applied its `trait_mod:<is>`
handler each time the declaration executed, after the `BEGIN` phasers of the
unit. The nested-scope lift of variable traits now admits a declaration with an
initializer, as the unit-level lift already did: the handler runs once in the
prologue over the variable's static cell and the assignment stays at its
position (#14146).

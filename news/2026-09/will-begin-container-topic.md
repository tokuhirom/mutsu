# `will begin` runs in the BEGIN prologue on the declared container

`my $x will begin { $_ = 3 }; say $x` died with "Cannot assign to an immutable
value", because the trait body ran with `$_` bound to the declaration's
initializer literal rather than the variable. A top-level `will begin` group is
now split by the unit prologue (ADR-0134): the declaration's static half and the
`begin` phaser go to the prologue in source order with `$_` bound to the
declared container, while the initializer and the other trait phasers stay at
run time. The nested (in a routine or block) form is still not lifted.

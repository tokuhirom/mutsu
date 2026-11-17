# A `do` block's tail variable is read after its LEAVE phaser

A `do` block and an EVAL'd unit yield their tail variable's container, which
is read after the LEAVE, KEEP and UNDO phasers have run.
`do { my $x = 3; LEAVE { $x = 4 }; $x }` is now `4`, as on rakudo. A KEEP
phaser for a defined tail and an UNDO phaser for an undefined one change the
value the same way. A routine still returns the value from before its
LEAVE, because a return decontainerizes first, so `sub { ...; $x }` stays `3`.
The block's value is decontainerized once the phasers are done, so assigning
it gives a copy.

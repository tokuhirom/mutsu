# Boxing `$!attr` into a list allocates less and skips a repeated type lookup

`method args { $!left, $!right }` puts the attributes' own containers into the
list it returns. Each such `$!attr` built three `String`s for the storage-key
probes and walked the class MRO for the attribute's type constraint on every
call, even when the attribute had been promoted to its container long before
(the constraint was registered then). The probes now share one buffer, and
the constraint is looked up only when the container is created. The
method-exit attribute reconciliation that such a frame then runs also stopped
`format!`-ing two probe keys per attribute.

A two-attribute `method args` went from ~38,600 to ~30,500 instructions per
call. Red's `Red::AST.WHICH` calls `$.args` for every node it hashes or
compares.

# A closure over a `my` declared in a method argument keeps its own slot

`$h.add: my $a = $n; @r.push: -> { $a }` stored `$a` env-only, so when a later
loop declared another `my $a` in a call argument the closure resolved the name
to that later declaration and read `Nil`. A scalar declaration written directly
as a method argument now takes a real lexical slot, as the plain-call form
already did (#12282).

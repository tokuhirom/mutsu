# Autovivification refuses read-only and container-less receivers

`$p.push(1)` on a read-only parameter `$p` that held an undefined value used to create a new array
in `$p`. Now it raises Rakudo's "Cannot assign to a readonly variable or a value". The same applies
to a `for` alias.

A name bound straight to a type object has no container to fill, so autovivification refuses it too
(ADR-11276 §9.175). This covers `my $x := Any`, a sigilless `my \s = Any`, and the bare type word
in `Hash.ASSIGN-KEY(...)`. The last one used to write a stray hidden variable called `Hash`.

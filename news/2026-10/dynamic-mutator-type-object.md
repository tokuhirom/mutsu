# A dynamic mutator name on a type word refuses it like the literal one

`my $m = "push"; List."$m"(1)` answered `[1]` where `List.push(1)` raises
`X::Immutable`: a dynamic method call on a bare word compiled to the
non-mutating call op, whose typed-array autovivification built a fresh array.
It now compiles to the mutating op, as the literal spelling does (#13888).
Both spellings of `Array.push(1)` on the bare type word now raise Rakudo's
`X::Multi::NoMatch` (`STORE(Array:U: Array:D)`): a bare `Array` type object
has no container to vivify; a variable holding one still vivifies.

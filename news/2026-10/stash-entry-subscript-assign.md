# Subscript assignment through a package stash entry stores into the entry

`Foo::<%reg><x> = 5` died with "it returned Stash, not an Array or Hash
container": the lvalue-chain root temp held the stash itself, which is no
container to store through. The temp now holds the stash *entry* (`Foo::<%reg>`,
the shared `our %reg`) and the chain continues below it, so hash, array and
deeper chains store in place as on rakudo (#15486).

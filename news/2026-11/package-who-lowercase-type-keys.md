# A lowercase class is a package member `c1`, not a `$c1` scalar

Slice of #16819. A package stash decided "type or scalar" from the first letter of an `our`-store
key (`C1` is a type, `c1` a `$`-scalar), so `module M { class c1 { } }` listed `$c1` beside `c1`.
A value that is the type object of its own qualified name is a type whatever its case. An exported
scalar also no longer appears twice (`v2` beside `$v2`) in the defining package's stash.

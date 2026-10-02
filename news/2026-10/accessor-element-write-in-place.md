# An accessor element write keeps a bound alias of the attribute

`class H { has %.u is rw }; my $t := $a.u; $a.u<o> = 7; $t<o> = 1` left
`$a.u` at `{:o(7)}`. Rakudo has `{:o(1)}`, since the alias and the attribute
are one container (#10897).

The element write through the accessor (`builtin_index_assign_method_lvalue`)
rebuilt the hash and moved its holders onto the copy by identity. That reached
the env and other instances, but not a `:=` alias held in a local slot, which
stayed on the old container. When the accessor hands back the attribute's own
`Hash`/`Array` (the same `Gc` as the instance's attribute slot), the element
now goes into that container in place, so every alias sees it. An object hash
records the key object as it stores. Accessors that return a copy or a
computed container, or that take arguments, keep the general path.

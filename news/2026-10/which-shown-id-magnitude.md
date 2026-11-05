# `.WHICH` of a reference object shows an address-sized number

`Array`/`Hash`/`Seq`/`Slip`/instance/code/type-object `.WHICH` showed a tiny
counter (`Array|1`, `C|20`); rakudo's is a 13-digit address (`Array|3168270547864`)
and Data::Dump::Tree tells a real address from a fallback by its digits. The ids
stay small and never reused internally; the number shown in the `.WHICH` string
(and the object-hash key that mirrors it) is offset to the same magnitude
(#15626).

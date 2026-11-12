# `.WHAT` on a collection and multi-parameter `for` batches take short paths

Two costs that EEL's per-fork state copier (`EEL::State.copy`, #17530) pays
for every value it copies.

`.WHAT` is a macro in Rakudo, and the pseudo-method lane (ADR-15829) already
answered it directly for a plain scalar or a type object. A `Hash`, `Array`,
`Pair`, `Rat`, `Range` or `QuantHash` value took the generic method path
instead, about 31k instructions per call, although that path ends in the same
`dispatch_what`. The lane now admits these values too; a typed or shaped
container keeps its parameterized type, which `dispatch_what` reads off the
value. `.HOW` on them still takes the generic path.

A multi-parameter loop (`for %h.kv -> $k, $v { }`) binds its parameters with
compiled statements that read the per-iteration batch. The batch used to live
in the environment under a hidden name, so each iteration paid a hash insert
and each read a by-name scope walk. The compiler now gives it a local slot.

Instructions per operation (release build, callgrind): `EEL::State.copy` of a
three-key `Hash` 2.20M to 1.63M, `for @six -> $a, $b { }` 113k to 91k.

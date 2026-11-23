# `.unique` and `.repeated` stay linear on Seqs, Sets, Versions and allomorphs

`.unique` and `.repeated` keep an index of the values seen so far, hashed by
their `===` identity class. Until now only numbers, strings, Pairs, Arrays,
Hashes and plain objects had a hash. Every other kind was compared with every
value seen, so `.unique` over 4,000 distinct Sets took 5.9 s on a debug build,
and the time quadrupled with each doubling of the list (#9161).

The index now hashes Seqs, Slips, enum values, Set/Bag/Mix, Versions,
routines, regexes, type objects, `Nil`, integer Ranges, allomorphs and other
mixins over a hashable value, ObjAt and metaobjects. A container hashes as the
value it holds. Arrays and Hashes hash by their `.WHICH` id, which a read
snapshot shares with its node, instead of by node address. A kind with a
cross-kind identity rule (`int === 0`, `Nil` the type and the value) takes the
other kind's hash.

On the debug build, 4,000 Sets now take 0.007 s, Versions 0.010 s, Seqs
0.005 s and IntStr allomorphs 0.02 s, and every case grows linearly. A
Junction, a by-name routine handle, a Capture and a few rarer kinds are still
compared with everything seen, so the issue stays open for them.

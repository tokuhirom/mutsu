# Reading `IterationEnd` no longer assembles the whole GLOBAL stash

A bareword nothing in scope claims was resolved by asking GLOBAL's stash for it,
and a stash read of GLOBAL builds the entire stash (every env key, routine and
class) to answer one key. The core `IterationEnd` sentinel is such a bareword: it
sits at the end of the resolution chain, so every `pull-one` that returned it, and
every `=:= IterationEnd` test of a consumer, paid that build. With Red loaded one
read cost ~1.8 ms; it is found in CORE before GLOBAL is consulted in Raku, so the
GLOBAL probe now skips it. Two smaller costs on the same path went too: the
my/GLOBAL stash hiding test built a `String` per hidden `need` class for every env
key it looked at (now one set probe per `::` position in the key). One
`IterationEnd` read under Red: 12.0M -> 6.4M instructions from the hiding test
alone (profiling build, callgrind window), and the GLOBAL build removed on top.
Refs #15498.

# The GC's white-subgraph walk drops its redundant visited set

`collect_white` gathered each cycle-garbage component with a work list and also kept a
`HashSet<usize>` (std SipHash) of visited node ids. A node is taken only while it is White and is
coloured Black the moment it is taken, so a second path finds it Black and skips it: the set held
exactly the Black nodes and its `insert` on the entry node always answered "new". It cost one
SipHash per collected node, ~37M Ir (about 5%) of a Red `Bla.^create`, plus rehashes. The set is
gone; the walk is unchanged otherwise (still O(n + e) over the white subgraph).

Real Red 0.2.7, profiling build, callgrind slope N=5 -> 15 of `Bla.^create`: 678.8M -> 628.4M Ir per
create (-7.4%; -10.3% together with #15926 against 700.4M).

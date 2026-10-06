# An integer atomic refuses an untyped variable passed to an `is rw` parameter

```raku
sub bump-rw($p is rw) { $p⚛++ }
my $plain = 1;
bump-rw($plain);     # accepted, $plain was now 2
```

Rakudo refuses this ("Cannot resolve caller postfix:<⚛++>(Int:D); the following
candidates ..."): `$p` is the caller's plain Scalar, and an integer atomic wants a
native-integer container. mutsu ran it, because the cell an `is rw` parameter
receives for an *untyped* caller variable has no `of`-type -- and so does the cell
of a native variable whose type no promotion site copied onto it. A check on "the
cell has no constraint" would have refused `my atomicint $x; my $y := $x; $y⚛++`,
a correct program, so the run-time check stayed lenient there (#11834, #12007).

The cell now says it. The site that creates a cell from a variable's own slot --
an `is rw` argument, a captured lexical, a `:=` source -- looks at the variable's
declaration, and when it is untyped marks the new cell `declared_untyped`
(`ContainerCell`); `Interpreter::builtin_atomic_int_target` refuses a cell with that
mark exactly as it refuses one with a non-native `of`-type. Only a cell *created*
from the variable is marked. A cell merely reached by another name -- an alias, an
`is rw` parameter relayed through another one, a sibling closure's capture -- is
never marked, because the reaching name carries no type either way; those cells
still answer "unknown" and the atomic runs. Native variables (`int`, `atomicint`,
`int64`, `state`, several declared together, captured by a closure, shared with
threads) all reach their atomics unchanged.

Still lenient, filed separately: an untyped variable a closure only *reads* by name
(never promoted to a cell) and then passes to an `is rw` parameter.

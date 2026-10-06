# An integer atomic refuses an untyped variable passed to an `is rw` parameter

```raku
sub bump-rw($p is rw) { $p⚛++ }
my $plain = 1;
bump-rw($plain);     # accepted, $plain was now 2
```

Rakudo refuses this ("Cannot resolve caller postfix:<⚛++>(Int:D); the following
candidates ..."): `$p` is the caller's plain Scalar, and an integer atomic wants a
native-integer container. mutsu ran it, because the cell an `is rw` parameter
receives for an *untyped* caller variable has no `of`-type -- and neither does the
cell of a native variable whose type no promotion site copied onto it. A check on
"the cell has no constraint" would have refused `my atomicint $x; my $y := $x;
$y⚛++`, a correct program, so the run-time check stayed lenient there (#11834,
#12007).

The cell now says it. A site that creates a cell from a variable's own slot looks
at the variable's declaration and, when it is untyped, marks the new cell
`declared_untyped` (`ContainerCell`); `Interpreter::builtin_atomic_int_target`
refuses a cell with that mark exactly as it refuses one with a non-native
`of`-type. The marking sites are the ones that box a variable from its own frame:
an `is rw` argument (`capture_var_cell_with`), a captured lexical
(`box_captured_lexicals`, `box_decl_local_cell_any_sigil`), a `:=` source, the
`is rw` binder when the caller's variable never had a cell (a free variable a
closure only reads), and the stores that hold a module's own file-scope and
`module M { }`-block variables.

Only a cell *created* from the variable is marked. A cell merely reached by another
name -- an alias, an `is rw` parameter relayed through another one, a sibling
closure's capture -- is never marked, because the reaching name carries no type
either way; those cells still answer "unknown" and the atomic runs. Native
variables (`int`, `atomicint`, `int64`, several declared together, captured by a
closure, `:=`-aliased, relayed through two `is rw` parameters, a module's own)
reach their atomics unchanged. A rebound variable's binding cell is looked through
to the container it is bound to.

Found on the way and filed separately: a native bumped through an `is rw`
parameter from `start` blocks alone loses its updates (#12042), and an element
atomic after the first `start` is a no-op (#11833).

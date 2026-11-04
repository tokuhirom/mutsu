# A plain Int, Num, Str or Rat answers "not Iterable/Positional/Associative" without the role gauntlet

`nqp::istype($pos, Iterable)`, which every `CArray` index read runs on its index, took about
9k instructions to say that an `Int` is not `Iterable`: the general checker walks the role-key
resolution, the MRO and the composed-role closure before it can only say no. A plain `Int`,
`Num`, `Str` or `Rat` against `Iterable`, `Positional` or `Associative` is now a constant-time
miss, taken only when the catalog says the class does not do the role and the class composes
nothing the catalog does not list (an `augment`/`.^add_role` composition falls back to the full
check).

Measured with the callgrind slope (profiling build, second run, 500 -> 1500 reads of
`$x = $c[$_ % 500]` on a `CArray[num64]`): 118.0k -> 112.0k Ir per read (-5%). Refs #16373.

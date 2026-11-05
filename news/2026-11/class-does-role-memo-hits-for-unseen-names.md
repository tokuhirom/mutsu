# The `class_does_role` memo also serves names it had never interned

The memo added for the `Sequence` delegate looked its class and role names up in the symbol table
and, when a name had no symbol yet, fell back to the uncached walk, for every call. A direct
`$carray.AT-POS($i)` asks the question twice per call with such a name, so the walk
(`collect_roles_for_class`) still ran on each call. A name with no symbol is now interned once
(a name already known is still only looked up, which `named_call_intern_budget` pins), so its later
asks hit the memo.

Callgrind slope (profiling build, second run, 500 -> 1500 calls of `$x = $c.AT-POS($_ % 500)` on a
`CArray[num64]`): 32.2k -> 31.1k Ir per call. Refs #16215.

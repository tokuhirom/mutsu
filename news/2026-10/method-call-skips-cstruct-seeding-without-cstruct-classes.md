# An instance method call no longer probes for a CStruct handle unless the program has one

`call_compiled_method_fast_in_unit` called `seed_cstruct_fields_for_method` on every
instance-method call, which looked for an `address` attribute in the invocant's cell by
string key before deciding the class was not a CStruct: ~455 Ir per call (#14639). It now
asks the registry first whether any `is repr('CStruct'|'CPPStruct'|'CUnion')` class exists
at all, the check `install_cstruct_storage` already uses, and returns before touching the
cell when none does.

callgrind (profiling build, 100,000 `$o.m()` calls): `seed_cstruct_fields_for_method`
462 -> 43 Ir per call inclusive; the 43 left is the registry read guard.

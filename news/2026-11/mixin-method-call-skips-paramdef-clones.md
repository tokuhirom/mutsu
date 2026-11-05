# A role method with a leading invocant no longer clones its ParamDefs per call; plain core constraints skip the type walk

`call_compiled_method_in_unit` cloned every `ParamDef` and name of a method on each call whenever
the method declared an explicit invocant (`::?CLASS:D:` on every CArray role method); the invocant
is the leading parameter, so the rest of the signature is now bound as a borrowed slice. And a
plain `Int` / `Num` / `Str` argument checked against its own core type or one of its core ancestors
(`Int:D`, `Any:D`, `Cool`, ...) is answered before the full `type_matches_value` walk (which resolves
aliases, type captures and package-scoped names) -- it only ever answers yes, every other shape still
takes the full check.

Callgrind slope on a profiling build, second run: fill (`@c[$_] = 1.5e0`) 101k -> 89k Ir per store;
fill + `.Array` 219.6k -> 198.6k Ir per element. Refs #16215.

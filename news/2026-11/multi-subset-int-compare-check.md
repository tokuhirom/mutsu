# A subset whose predicate is one integer comparison costs one comparison in multi dispatch

A multi candidate typed with `subset Small of Int where * < 100` (or with
`UInt`) used to be checked on every call by name: resolve `Small` in the
current package, look up its term binding, intern names, check the base type,
then run the predicate. That cost more than the rest of the dispatch, and it
also kept the family off the multi light call, which only takes programs whose
checks cannot run user code. The dispatch program built once per
argument-type key now resolves such a subset to its comparison and answers its
base type from the key (ADR-18580 D1). A per-call check is then one integer
comparison, and anything else (a big integer, a user override of the
comparison operator) falls back to the full check. `bench-multi-dispatch`'s
subset section goes from 693M to 583M instructions.

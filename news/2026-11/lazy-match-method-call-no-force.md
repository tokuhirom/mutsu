# A method call on a Match no longer materializes it

A regex match is a lazy `Match`: the capture node the matcher built, with the
Instance-shaped attribute map built only when something asks for it
(ADR-0016 P5). Reads such as `.Str`, `.from` and `.made` are answered from the
node. But any method call on a lazy Match, `$/.values` or `$/.ast` in a
grammar's `{ make ... }` block for instance, forced that materialization
anyway. The method-call path probes its receiver before dispatching it, and
each probe used `view()`, which materializes a lazy Match. The probes were the
Array, Hash, QuantHash and native-scalar subclass delegates, `descalarize`, the
mutating-row owner lookup, the native-array result retyping and the sink of a
statement value.

Those probes now recognize a lazy Match by its tag and skip it, since a Match
is none of those kinds. The node also memoizes its positional list and named
hash on their own. A later materialization reuses them, so `$/[0]`, `.list`
and `.values` still hand out the same child Matches, and `Capture.values`
reads them without forcing.

One warm `load-yaml` of `bench-yaml-parse-big`'s document, `MUTSU_JIT=off`,
callgrind: 120.07M -> 118.94M instructions and 77.7k -> 75.7k allocations
(#12917). Every `Schema::Core.new.parse` that concretizes a plain scalar
calls `$/.values[0].ast` in its `TOP` block.

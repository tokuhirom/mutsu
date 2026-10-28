# The pointy `given` / `with` parameter markers are `VarMarks` fields

The parameter declaration a pointy `given` / `with` block gets (`given $x -> $v is copy { ... }`)
carried three `__`-prefixed entries in its `custom_traits`: `__pointy_copy`, `__pointy_native_param`
and `__restore_pointy_given_topic`. They are `VarMarks::{pointy_copy, pointy_native_param,
restore_pointy_given_topic}` now, next to the other facts about a `VarDecl` that are not traits.

Deleted: the three trait spellings and their `custom_traits.iter().any(…)` scans in the `given`
compile, the synthetic-block compile and the `with` expression compile.

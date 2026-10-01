# A closure calling a nested `my sub` by name ignores the caller's readonly param

A closure that only calls a nested `my sub` by name, where the sub writes a
captured variable, no longer sees a same-named readonly parameter of its
caller. The compiler now records the variables each nested sub writes
(transitively), folds them at call sites into `CompiledCode::nested_sub_written_free`,
and the readonly-capture reconcile includes that list. Fixes #10400.

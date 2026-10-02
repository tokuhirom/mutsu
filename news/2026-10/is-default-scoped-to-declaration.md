# `is default(...)` is scoped to its declaration

A lexical's `is default(...)` value used to live in one process-wide table keyed
by the bare variable name. That was wrong in both directions: a block's
`my $x is default(3)` leaked into an outer (or class-body) `$x` once the block
had run, and a later unrelated `my $x` erased the default of a still-live `$x`
that a closure had captured.

The default now lives in the env under `__mutsu_var_default::<name>`
(`MetaNs::VarDefault`), next to the declaration's type constraint, so it is
scoped and captured exactly like the variable itself. Attribute defaults keep
their name-keyed table: they are registered under twigil names (`!x`, `.x`, ...)
that no lexical can carry. (#10796)

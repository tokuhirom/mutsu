# The built-in exception classes' composed roles are computed on first lookup

At start-up, `build_builtin_registry` used to fill three composed-role registries:
`class_composed_roles` (the flattened set), `class_direct_composed_roles` and
`class_does_only_roles`. It wrote an entry in each for every static-catalogue row that
composes a role, about 220 `X::` classes, with a `Vec` of cloned `String`s per entry. That was
about a fifth of `say "hello"`'s instructions, for role lists that only `.^roles`, `~~` or
role dispatch on one of those exceptions ever reads.

The three registries are now `ComposedRoleTable`s (ADR-19206 slice 3). This is the same shape
as the class table:

- **Overlay**: a copy-on-write overlay holds every entry written at run time, plus the eagerly
  built ones.
- **Catalogue entries**: an entry from the catalogue is computed on its first lookup, into a slot
  shared by the whole process. The flattened set walks the built-in `role_parents` as it was
  frozen at the point where the eager loop used to read it.
- **Writes and rollback**: a write copies the entry into the overlay, a removal hides the row,
  and an EVAL rollback restores both.
- **Test**: a unit test checks every row against the eager computation, including
  `X::TooLateForREPR`, whose composed role is also its parent.

Measured on `benchmarks/bench-startup.raku` (`MUTSU_JIT=off`, CI-style release build) against
`main` at `5f44cb0d`, with faults and wall time over 2×40 interleaved runs:

| | before | after |
|---|---:|---:|
| instructions, whole run | 5.43M | 4.41M (-19%) |
| instructions, `build_builtin_registry` | 2.09M | 1.07M |
| minor faults | ~1,038 | ~987 |
| median wall time | 5.70 / 5.37 ms | 5.19 / 5.16 ms |

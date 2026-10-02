# An `is export` sub nested in a block of a class body is exported

`class K { if True { sub g is export { 1 } } }; import K; say g()` died with
`No exports found for module: K`. The BEGIN prologue splits a class body whose bare
statements run at run time, so the nested routine lived only in the
`PackageRuntimeBody` half, which the CHECK-time inline-package prepass did not enter, and a
top-level class was not entered at all. The prepass now enters package-scoped class bodies
(declaration half and run-time half) at any nesting for their exported routines.

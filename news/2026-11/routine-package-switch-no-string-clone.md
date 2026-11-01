# A routine call into another package no longer clones the package name

`enter_routine_package_outlined` switched the current package of a call into a
routine declared in another package (every imported routine, every module
sub) through `set_current_package_with_sym(cf.package.clone(), sym)`. The
`String` parameter was read only by a `debug_assert`, so each such call
allocated and freed a copy of the package name for nothing. The function is
gone: its five callers (the routine and catch-handler package switches, the
attribute-`where` scope, the class-method loader) call
`set_current_package_sym(sym)`, which is what the package state is stored as.
Measured on a loop calling an imported `sub inc($x)`: 6,059 -> 5,845 Ir per
call (profiling build, callgrind second run); a call of a routine declared in
the program is unchanged. Part of #14371.

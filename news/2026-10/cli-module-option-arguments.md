# -M takes import arguments

`-M'Module arg,list'` (and `-M 'Module arg,list'`) is `use Module arg,list;`,
written on the program's first line so the module's `EXPORT` addresses the
program's own unit through `$*CU` and every line number is kept. Before, the whole
string was looked up as one module name. A bare `-MModule` loads as before.
`static`'s `bin/static-check.raku` runs `-Mstatic::CLI "fail-on-mismatch","1","1"`.

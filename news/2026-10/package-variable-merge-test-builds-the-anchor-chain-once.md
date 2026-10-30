# Reading a package variable no longer re-walks the EVAL unit chain per granting module

`unmerged_package_var_key` decides whether a qualified variable (`Red::Str`,
`$Pkg::x`) belongs to a module that is not merged into the running code. It asked
`module_merged_here` for every module that had granted the top package, and each
answer re-walked the two anchor units' `EVAL` parent chains (a global `RwLock`
read per step). A package granted by a hundred modules, such as `Red`, paid a
hundred walks per read, and `get_our_var` runs on every attribute-default and
type-name resolution. The chain does not depend on the module, so it is now built
once per read (`merge_anchor_units`, shared by the single-module and the set
form). `Red::Column.new` (22 attributes with default expressions) went from
322.9M to 155.4M instructions per 100 constructions in a `profiling`-build
callgrind window (-51.9%). Refs #15498.

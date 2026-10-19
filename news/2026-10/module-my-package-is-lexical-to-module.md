# A module's `my package` is its own; the importer's `$P::x` is a separate variable

A `my package P` declared in a module is lexical to that module. In rakudo, a
script that names `$P::x` gets a separate `P` autovivified under its own
GLOBAL, so the module's `$P::x` and the script's are two variables. mutsu
kept every package variable under its flat qualified name, so a script's
`$P::x = 'main'` overwrote the state the module kept there. UserTimezone keeps
its timezone that way (#11156).

The compunit that declares each `my package` is now recorded. A qualified
variable of such a package, accessed from any other compunit, goes to the
separate store that #13942 introduced for packages a block's `use` did not
merge.

# Stores through a package stash reach the package's `our` variable

A package's `our $v` is one container that its stash publishes. `Pkg.WHO<$v> = 1`, `$stash<$v>++`, `--$stash<$v>`,
`$stash<$v> += 5` and `my $c := $stash<$v>; $c = 7` used to write into a copy and leave the variable alone, so
CustomImporting-style code that imports a package's variables through `.WHO` never saw its own writes. They now store
through the variable's cell, and a `$stash<$v>` bind source keeps the cell.

The `.VAR` descriptor of a stash symbol (`Pkg.WHO<$v>.VAR.name`, `.WHO<@a>.VAR.name`) is named by the symbol's key.
`Pkg::<$v>.VAR.name` and the name a variable bound with `:=` to such a cell reports remain for #16923.

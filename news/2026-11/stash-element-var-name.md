# `.VAR.name` of a stash symbol and of a variable bound to an `our` cell

`Pkg::<$v>.VAR.name` and a variable bound with `:=` to a package stash cell
(`my $c := Pkg.WHO<$v1>`) or to an `our` scalar now report the declared name (`$v1`), as
rakudo does: `our` scalar cells are named at declaration, and `Pkg::<...>` subscripts
take the same element-descriptor path as any named container (#16923).

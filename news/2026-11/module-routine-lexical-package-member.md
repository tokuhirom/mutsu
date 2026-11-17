# A module routine names its own lexical package's enum members and constants

A module routine could name a type nested in the module's own `my class` or
`my package` by its qualified name, but not an enum member or an
`our constant` of it, once the routine was called from the program:

```raku
# KnM.rakumod
my class Enums { our enum Status (Ok => 0, Bad => -1); }
my package Consts { our constant Status = 5; }
our sub f() is export { Enums::Status::Bad }
our sub g() is export { Consts::Status }
```

`f()` died with `Could not find symbol '&Bad' in 'GLOBAL::Enums::Status'` and
`g()` with `Could not find symbol '&Status' in 'GLOBAL::Enums'`; rakudo gives
`Bad` and `5`. An enum member is now built from the enum registry once the
module's scope no longer holds it (one helper, shared with the `Pkg::<key>`
stash read), a package's `our constant` is read from the package-symbol store,
and a `my package` with no types records its head for the module's routines
like a `my class` does (#17720).

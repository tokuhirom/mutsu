# `our sub` of a package declared in EVAL is callable afterwards

`EVAL q[package Q0 { our sub v { 0 } }]; say Q0::v()` printed `Could not find symbol '&v' in
'Q0'`. The unit's routine table is rolled back when the EVAL returns, but the `our sub` is
published to the package stash, which the caller shares. A package-qualified call that finds
nothing in the routine table now consults the persistent our-scoped table before giving up.

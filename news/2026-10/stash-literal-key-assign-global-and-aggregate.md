# Literal-key stash assignment: `GLOBAL::<$g>` and `Pkg::<@a>`

`GLOBAL::<$g> = v` now writes `$GLOBAL::g` instead of silently dropping the store, and
`Pkg::<@a> = v` / `Pkg::<%h> = v` now dies with "Cannot assign to an immutable value" like
Rakudo (#10425). Binding `Pkg::<@a> := ...` is not yet supported and remains separate work.

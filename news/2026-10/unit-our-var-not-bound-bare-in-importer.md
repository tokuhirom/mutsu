# A unit module's `our` variable no longer leaks into the importer's scope

A `unit module P` body runs in the loading scope's env, and its `our $x`
binding stayed there afterwards: `use P; say $p-our` printed the module's value
for a variable P never exported, where rakudo says it is not declared. Because a
block-scoped `use` is preloaded at the head of the unit, the leak also made an
exported `our` visible file-wide, even from a block that never ran.

After a `unit` compunit's body runs, the loader now drops the bare `env`
bindings of its ordinary `our` variables (scalars, arrays and hashes, exported
or not), as it already did for its `constant`s and enums. `$P::x` still reaches
the variable, `import_module` still installs an exported one in the scope of the
`use` that asked for it, and the module's own routines keep resolving the bare
name to the package cell (`vm_our_package_vars`). Unlike a constant, an `our`
variable gets no `unit_lexicals` alias, which would outrank a routine's own
`my $x` captured by a closure (#11009). The two TODO checks in
`t/modules/module-import-alias-scope-paths.t` now pass.

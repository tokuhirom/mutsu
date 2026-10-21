# EVAL pre-runs its top-level `use` and `import` before the CHECK-time checks

An `EVAL`'d unit now runs every eligible top-level `use` (and an `import` of a
module an earlier `use` loaded) before its CHECK-time validation, as rakudo does
at BEGIN time, instead of only the `use`s of modules that import through a
`sub EXPORT` hook (#13441). Which statements qualify is decided by one shared
function, `compiler::begin_use::loads_module_in_position`, that mirrors the
special cases of the compiled `use` arm: pragmas, `Test`, `:from<NQP>`,
`:if(...)`, an empty import list and a bareword argument are left to the
compiled unit.

Only a hook module's statement is dropped from the unit; the others stay so the
in-position run still records the lexical import set the unit's by-name calls
read (ADR-8751). The static source harvest is still consulted; retiring it for
the pre-run statements is the next slice.

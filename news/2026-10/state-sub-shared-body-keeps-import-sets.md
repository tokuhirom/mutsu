# A `state`-declaring module sub keeps its imported routines when called qualified

A module sub that declares `state` (also implicitly, e.g. `"{$0}"` inside a
string) gets a shared body captured at load (`imported_compiled_fns`), which a
qualified call (`Mod::sub(...)`) runs. That body was compiled as a plain block,
without the module's lexical import sets, so its calls to an imported `proto`
resolved by bare name in the caller's scope and died with "Routine does not
have any candidates". The capture now compiles the module as its mainline is
compiled. Found via JavaScript::Google::Charts (`DataTable::generate-code` ->
`Data::TypeSystem::is-reshapable`): its `t/scatter-column-names.rakutest` now
matches raku (8/8; was a green -> red regression).

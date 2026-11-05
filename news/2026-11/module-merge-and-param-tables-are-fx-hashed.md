# The module-merge lookup tables and the parameter-bound aggregate table are Fx-hashed

`Interpreter::module_merged_in_units` runs ~1,365 times per AttrX::Mooish iteration (every
variable read that asks whether a package-granting module is merged into the running unit), each
time through two std `HashMap` lookups (`module_units: Symbol -> Symbol`, `unit_merged_modules:
Symbol -> HashSet<Symbol>`) that SipHash a `Symbol` id. `ParamBoundAggregates::note`, run for
every `@`/`%` parameter bind, did the same with a name and a container address. All keys are
internal ids, names and heap addresses, never program input; the tables are `FxHashMap` /
`FxHashSet` now (`rustc-hash` was already a dependency and `loaded_unit_set` next to them already
used it).

`scripts/call-path-ab.sh mooish-lazy` (profiling build, callgrind slope), per-function Ir per
iteration: `RandomState::hash_one` 173k -> 53k, `sip::Hasher::write` 92k -> 53k,
`module_merged_in_units` 58k -> 29k, about -150k Ir (-3.7%) in total. The whole-iteration
slope read 4.00M before and 4.04M after: inside this loop's build-to-build spread (about +-5%,
see #15688), so the whole-loop figure does not show it; the function-level figures do.

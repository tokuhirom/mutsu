# `src/vm/` builds and classifies qualified names once per symbol

95 of the 98 run-time package-name string surgery sites under `src/vm/` are
gone: `format!("{pkg}::{name}")` joins, `== "GLOBAL"` compares and
`contains`/`rsplit_once("::")` scans. Each one now goes through the memoizing
constructors in `src/qualified.rs` and uses the `Symbol` its caller already
holds. That covers a routine's `LEAVE` key, a bareword's `Pkg::tail` split,
the `GLOBAL`-package gates on the call paths and the type-declaration
qualification. Each is now decided once per symbol, not on every execution.

`src/qualified/split.rs` adds the read-side helpers these sites needed:
`split_qualified`, `last_segment`, `is_inside_package`, `segments` and
`is_type_capture`.

Three byte scans stay, in `unit_lexical_slot`/`unit_lexical_slot_mut` and
`our_package_var_key`. Those resolvers run on free-variable reads and hold
only the name's text. Interning the name there instead cost 24 interns per
`Test` assertion and broke `tests/named_call_intern_budget.rs`. Removing them
means passing the callers' symbols down.

The `src/vm/` rows of `scripts/name-scans-baseline.txt` now record only those
three sites. This is one slice of #11507.

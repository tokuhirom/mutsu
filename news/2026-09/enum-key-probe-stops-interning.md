# Resolving an enum type key no longer interns the name it probes

`resolve_enum_type_key` (#9716) looked the type name up in the env with `env.get(name)`. That
call interns the name every time it runs. The lookup runs on every coercion to a non-enum type,
such as the `Bool(Mu)` coercion of each `Test` assertion. It added one intern per assertion,
which pushed `tests/named_call_intern_budget.rs` to its 12-per-assertion budget and turned
`main`'s `test-check` red.

A name that was never interned cannot be an env key, so the probe now uses `Symbol::lookup`
first. A `Test` assertion is back to 10.98 interns.

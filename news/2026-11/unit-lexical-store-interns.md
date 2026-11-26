# A store to a unit-level lexical no longer re-interns its name

A routine that writes a lexical declared at its compunit's top level, as
`Test.rakumod`'s `proclaim` does with `$num_of_tests_run`, `$time_before` and
`$time_after` on every assertion, interned the written variable's name once
per store. It interned it a second time once any `:=` bind in the program had
armed the sigilless-alias probe.

Rakudo 2026.11's `Test.rakumod` adds such a bind at its top level
(`my %ENV := %*ENV`), so swapping it in doubled the cost of every assertion
from 7 to 14 interns (#19215, found while reviewing the Rakudo-Core re-vendor in
#19210).

Both probes now look the name up instead:

- `MetaNs::key_for_str` goes through `qualified::known_symbol`.
- `package_alias_var_name` uses `is_qualified_str`.

The name is a compiled constant that is already in the symbol table, so the
store interns nothing. A `Test` assertion now costs 4 interns under either
`Test.rakumod`, and the budget in `tests/named_call_intern_budget.rs` drops from
10 to 5.

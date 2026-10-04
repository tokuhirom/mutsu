# `check-name-scans` is a ban: no run-time qualified-name string surgery is left

Issue #11507 is closed. The last eight sites were byte scans on hot paths that
had only a name's text: `unit_lexical_slot`/`_mut`, `our_package_var_key`,
`type_matches`, `package_type_alias`, `module_scope_lexical` and
`module_imported_lexical`. They now ask the name's symbol:

- `unit_lexical_slot` and the `unit_scope_lexical*` family take an
  `Option<Symbol>`. The by-name read, the `SetGlobal` store, the env write and
  the read-only check pass the symbol they already hold. Other callers pass
  `None`, and the symbol is then looked up only after the empty-store
  early-outs.
- The three module-table lookups probe their table's key union before the
  qualification test, so the symbol lookup runs only when the name is
  actually in the table.

Callgrind on the release binary, measured against the scans:

- a 2,000-assertion `Test` loop: 477.2M → 476.9M instructions;
- a typed-binding and `Buf.push` loop: 1,872.8M → 1,876.9M (+0.2%, the two
  memoized flags `type_matches` now reads).

With every counter at zero, `scripts/name-scans-baseline.txt` is deleted.
`make check-name-scans` now fails on any match, as `check-magic-keys` has
since #8087. The sweep covered 577 sites in seven PRs:

- #11522 (the per-file baseline)
- #11723 (`src/vm/`)
- #11752 (types, regex, value, RakuAST)
- #11769, #11785, #11801 (`src/runtime/`)
- this change

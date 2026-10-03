# VM free-variable fallbacks build qualified keys once

The VM's fallbacks for bare and package-qualified free-variable reads used to
rebuild names from strings on every miss. They `format!`ted `<sigil><pkg>::<name>`
keys, split names with `rsplit_once("::")`, walked package chains one string
split at a time and compared packages against the string `"GLOBAL"`. The
affected paths:

- `package_chain_var_fallback` and `read_package_scope_var`;
- the `$D2::d3` nested-package shorthand;
- the class-body auto-qualified reads;
- the `Main::` alias;
- the unit-lexical resolvers.

They now work on the symbols the bytecode already holds:

- `src/qualified/var.rs` adds `qualified_var(pkg, name)`, the sigil-aware
  counterpart of `qualified()`, and `split_qualified_var(name)`, its inverse.
  Both are memoized per symbol, and a split borrows the interner's
  `&'static str`.
- `package_chain_var_fallback`, `read_package_scope_var`,
  `resolve_enum_member_in_current_package`, `auto_qualified_bare_env_read` and
  `package_qualified_candidate` now take `Symbol`s, so the GetGlobal-family
  callers pass `code.const_sym(..)` instead of a string to re-scan.
  `package_qualified_candidate` used to intern both strings on every call just
  to key its memo; it now needs no memo of its own.
- Package classification is done with `is_global_package` /
  `is_routine_scoped_package` on the symbol, not by string comparison.

This removes 27 `check-name-scans` sites (#11507): 2 `qualify`, 8
`global-cmp` and 17 `scan`. The unit-lexical resolvers, which only hold a
name's text, keep their scan for now: interning there would cost more than
the scan it replaces (a `Test` assertion went from 9 to 38 interns). They
carry TODOs to take the caller's `Symbol` instead.

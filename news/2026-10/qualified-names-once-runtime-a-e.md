# Qualified names built once per symbol in `src/runtime/[a-e]*.rs`

The #11507 sweep reached its first batch of `src/runtime/` top-level files,
`accessors_*` through `eval_*`. All 111 of their run-time package-name string
surgery sites are gone. They covered:

- stash assembly (member classification, sub-package heads, `GLOBAL` checks);
- the pseudo-package prefix strippers;
- `require` aliasing;
- CStruct short names;
- function and proto resolution;
- the compunit visibility gate;
- `our` symbol keys for `END` seeding;
- the bare enum-key namespace.

Some of these paths run per resolution and hold only a name's text. For
them, `src/qualified.rs` gains `known_symbol` and `is_qualified_str`, which
look the name up and intern it only the first time it is seen. That way the
`*_intern_budget` tests keep their counts: `[+]` reduction would otherwise
have re-interned its operator name on every execution through
`infix_associativity`.

`strip_pseudo_packages` and `innermost_pseudo_is_package_only` now share one
segment-based `leading_pseudo_packages` walk instead of two copies of the
same prefix loop.

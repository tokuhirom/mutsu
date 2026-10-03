# Qualified names built once per symbol in types, regex, value and RakuAST

This continues the #11507 sweep beyond `src/vm/`. It covers `src/runtime/types/`,
`src/runtime/regex/`, `src/value/`, `src/rakuast/`, `src/trir/`, `src/builtins/`,
`src/profile/` and a few single files, removing 84 of their 89 run-time
package-name string surgery sites. Among them:

- grammar subrule resolution: the enclosing-package scope walk, the `Pkg::rule`
  split and the `scope::name` keys now go through `package_ancestors`,
  `split_qualified` and `qualified`;
- role resolution and routine-mixin keys;
- method signature type qualification;
- type-capture binding;
- enum `.raku`;
- the RakuAST name-part splitting.

`src/qualified/split.rs` gains `type_capture_name`, `stash_stem`,
`ends_with_segments` and `text_segments`. Every parameter-constraint `::T`
check now goes through `is_type_capture` / `type_capture_name`.

Five byte scans stay. They sit on paths that run per type check or per
free-variable read and hold only `&str`: `type_matches`,
`package_type_alias`, `module_scope_lexical` and `module_imported_lexical`.
Each carries a TODO to take its caller's `Symbol`. `src/str_scan.rs`, which
defines the `has_double_colon` primitive, is now exempt from
`check-name-scans` like the other constructor modules; every call of the
primitive still counts.

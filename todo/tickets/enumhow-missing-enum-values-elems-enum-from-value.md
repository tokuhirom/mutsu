# `EnumHOW` is missing `.^enum_values`, `.^elems`, `.^enum_from_value`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Metamodel/EnumHOW.rakudoc:139,148,157`).

## Root cause

`Perl6::Metamodel::EnumHOW` already implements `enum_value_list` (returns the ordered list
of enum-value objects, `src/runtime/methods_classhow_dispatch.rs:1280`) and is dispatched
through `dispatch_classhow_method`/`is_classhow_method`
(`src/runtime/methods_native_bypass.rs`). Three sibling introspection methods documented
in `Type/Metamodel/EnumHOW.rakudoc` are simply not in the `is_classhow_method` allow-list
(so they never reach `dispatch_classhow_method` at all — they fail generic method lookup
with "No such method"):

- `.^enum_values` — a `Map` from enum-key name to its ordinal value.
- `.^elems` — the number of enum values.
- `.^enum_from_value(ordinal)` — reverse lookup: the enum-key name for a given ordinal.

All three can be implemented the same way `enum_value_list` already is: read
`self.registry().enum_types.get(&type_name)`, which stores the `(key, value)` variant
pairs in declaration order.

## Minimal repro

```raku
enum Numbers <10 20>;
say Numbers.^enum_values;        # {10 => 0, 20 => 1}
say Numbers.^elems;              # 2
say Numbers.^enum_from_value(0); # 10
```

- `raku`: as commented above.
- `mutsu` (`target/debug/mutsu`): `.^enum_values` and `.^enum_from_value` both die with
  `No such method 'enum_values'/'enum_from_value' for invocant of type
  'Perl6::Metamodel::EnumHOW'`. `.^elems` is worse: it **deterministically crashes with a
  stack overflow** (`thread 'mutsu-main' has overflowed its stack`, exit 134, reproduces
  every run) — reduced to a two-line repro:

  ```raku
  enum Numbers <10 20>;
  say Numbers.^elems;
  ```

  `"elems"` is presumably not in `is_classhow_method`'s allow-list either, so the call
  falls through the generic method-dispatch chain instead of `dispatch_classhow_method`;
  something in that fallback chain for an `EnumHOW` instance (a `Perl6::Metamodel::EnumHOW`
  `Instance`) apparently recurses into itself. Whoever picks this up should confirm with
  `rust-gdb -batch` (per the repo's debugging guidelines) which dispatch path loops, since
  this is a crash (highest priority per the roast triage rules), not just a missing
  method.

## Affected files (starting point)

- `src/runtime/methods_native_bypass.rs` (`is_classhow_method` — add `"enum_values"`,
  `"elems"`, `"enum_from_value"`)
- `src/runtime/methods_classhow_dispatch.rs` (`dispatch_classhow_method` — add match arms
  next to the existing `"enum_value_list"` arm, reusing the same
  `self.registry().enum_types.get(&type_name)` lookup)

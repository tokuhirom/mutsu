# A free-variable read stops cloning the current package name

Third perf slice for `todo/deep/vendor-real-test-module.md`. Every read of
a compunit's file-scope lexical from inside one of its routines -- which is
what `$num_of_tests_run`, `$indents`, `$output` and friends are to the
vendored `Test.rakumod`'s `proclaim` -- resolves through
`unit_lexical_slot`, and every such read (plus the `package_scope_lexical`
probe that precedes it, and the write-side twins) began with
`self.current_package()`: take the `RwLock`, clone the `String`, drop it at
the end. Thirty-odd times per assertion. The same functions asked
`name.contains("::")` -- a searcher build each time -- three times per
read, and the package-keyed stores they probe (`unit_lexicals`,
`package_lexicals`, `module_scope_lexicals`, `package_type_aliases`) hashed
their `String` keys with `SipHash`, two lookups per package tier of the
`::` chain walk.

- The eight `current_package()` reads in `vm_env_helpers.rs` now take the
  `&'static str` off the atomic symbol mirror (`current_package_sym().as_str()`)
  that the hot dispatch path already uses; both writers of
  `current_package` keep the mirror in step.
- `unit_lexical_slot` and its `_mut` twin probe `::` once.
- The four `package -> name -> V` tables are `FxHashMap` at both levels,
  behind one `PackageKeyed<V>` alias next to `MAINLINE_UNIT_KEY`, and the
  two generic chain-walk helpers take that alias.

## Measured

Callgrind, 300 `ok 1, "x"` under `MUTSU_REAL_TEST=1`, one-assertion
baseline subtracted:

| | per assertion |
| --- | --- |
| before (after the named-call-path slice) | 397,305 Ir |
| after | 378,452 Ir |

-4.7% on this slice, -23.1% since the session started at 492,188.
`roast/S03-buf/write-int.t` under the real module: 10.3 s -> 9.8 s (median
of three) on the same box, 13.8 s at the start of the session.

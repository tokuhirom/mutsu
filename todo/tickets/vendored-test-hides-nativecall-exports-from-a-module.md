# Under the vendored `Test`, a module's `use NativeCall` loses `nativecast`

A module that does `use NativeCall` and calls `nativecast` inside a sub dies
with `Unknown function: nativecast` — but only when the program's assertions run
through the vendored upstream `Test.rakumod` (`MUTSU_REAL_TEST=1`, the mode
`todo/deep/vendor-real-test-module.md` is trying to make the default). Under the
native TAP provider the same file passes.

## Repro

```sh
cargo build --release
scripts/battery-testsuite.sh >/dev/null 2>&1     # fetches the battery clones
cd tmp/battery-testsuite/NativeHelpers__Blob
MUTSU_REAL_TEST=1 ../../../target/release/mutsu -I lib t/01-basic.t
MUTSU_REAL_TEST=0 ../../../target/release/mutsu -I lib t/01-basic.t
```

Vendored:

```
ok 3 - sizeof Blob
# You planned 24 tests, but ran 3
Unknown function: nativecast
  in sub BODY_OF at lib/MoarVM/Guts/REPRs.pm6 line 64
  in sub carray-from-blob at lib/NativeHelpers/Blob.pm6 line 69
  in block <unit> at t/01-basic.t line 19
```

Native: all 24 pass.

`lib/MoarVM/Guts/REPRs.pm6` has a plain `use NativeCall;` at line 9 and calls
`nativecast` at lines 16 and 64. The test file itself is a third compunit above
both.

## Why it is probably import scoping, not NativeCall

`NativeCall` is a native provider: `load_module` recognizes the name and
`register_nativecall_exports()` populates its export stash rather than running
any `is export` declaration. Loading the vendored `Test` adds a real module
compunit to the load chain, which changes what `push_import_scope` /
`pop_import_scope` (`src/runtime/runtime_module.rs`) see around the nested
`use NativeCall` — the most likely reading is that the outer scope pop removes
the inner module's imported names.

Start there rather than in `runtime/nativecall.rs`: `nativecast` resolves fine
in the same file under the native provider, so the function exists and is
registered; it is its *visibility from the module's own sub bodies* that is
lost.

## Blast radius

`NativeLibs`'s `t/01-basic.t` regresses in the same sweep (1 of 23) and is worth
checking against the same root cause. Both are bundled-library gate rows
(`scripts/battery-testsuite.sh`), which is a CI step, so this blocks the
provider switch.

# Rakudo's real `Test.rakumod` is vendored, behind `MUTSU_REAL_TEST=1`

The upstream `Test` module now ships in the repository at
`modules/Rakudo-Core/lib/Test.rakumod`, vendored verbatim from the
`rakudo-2026.06` release tarball (md5 `f34dec45d52ad099c37f42fdbd93e277`,
recorded in that directory's README alongside `Pod::To::Text`). `use Test` still
resolves to mutsu's native TAP provider by default; **`MUTSU_REAL_TEST=1`** loads
the vendored file instead.

This is step 2 of `todo/tickets/vendor-real-test-module.md`, and the switch is
what step 2 asks for: exercise the real module *without* removing the
interception. Every `t/` file and every roast file stands on `Test`, so swapping
the implementation swaps the foundation of the whole suite; step 3 flips it once
the residue is gone.

## What the switch replaces

The exercise previously ran under a throwaway copy in gitignored `tmp/`, with
`unit module Test;` rewritten to `unit module Test2;` to dodge the interception —
a measurement that died with the container and that nobody else could reproduce.
The vendored file is byte-identical to that copy apart from the rename, so the
whole alias sweep was genuinely testing the verbatim upstream file; it is now in
the repository, and `scripts/test-module-sweep.sh` drives it with the env var.

## Placing the file is not automatically inert

The parse-time module scan (`find_module_file`) is filename-based and searches
the bundled-battery paths, so it can see a vendored `Test.rakumod` even while the
runtime interception is untouched. Measured before relying on it: 300 sampled
`t/` files produced byte-identical output with and without the file present.

## The general bug the switch exposed

Three files passed under the old alias and failed under the switch. There are
two dispatch paths into the native TAP provider, and only `exec_call` had the
"an imported declaration wins over the native provider" guard added in
`news/2026-08/imported-test-routines-beat-the-native-provider.md`;
`call_function_fallback` did not. A source that merely *mentions* `NativeCall` —
in a comment is enough — gets NativeCall's prelude injected, and that is enough
to send a listop call down the other path. There the native `plan` answered,
recording a plan nobody ran against, so `finish()` reported

```
# You planned 14 test, but ran 0
```

on a file whose fourteen assertions had all passed and all printed. The two
implementations kept separate counters, exactly the failure shape the first fix
was written for. Both paths now decide on whether a *declaration* exists, not on
whether the name is a builtin.

Under the alias the bug was invisible: `use Test2` never puts `Test` in
`loaded_modules`, so the native gate was shut and the mis-dispatch fell through
to the module anyway. Running the real module under its real name is what made
it observable.

Both guards now go through one helper, `user_test_decl_beats_native`, which also
carries the single exception the rule needs: `skip` is both a Test directive and
a Raku list routine, so a user `multi skip($n, +values)` accepts
`skip 'reason', 2` on signature alone. It keeps the shape-based disambiguation
(`skip_call_is_list_skip`) the three `skip` dispatch sites already apply —
without it, `t/skip-user-multi-shadows-test.t`'s `subtest` lost its TAP SKIP
directive to the user's list routine.

## Where the campaign stands

The full sweep after this and `news/2026-08/eval-context-argument.md`:

| | at the start of 2026-08-01 | now |
| --- | --- | --- |
| pass under both | 2617 | **2693 / 2732** |
| regress under the real module | 86 | **26** |
| passes only under the real module | 1 | 1 |
| fail under both (pre-existing) | 13 | 12 |

The 26 split 6 that `raku` also fails (test files to correct) and 20 real mutsu
gaps. Exit status was checked for the first time and is already faithful: a
failing assertion exits 1 and a short plan exits 255, which is what `prove`
reads.

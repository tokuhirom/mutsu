# Retire the native `Test::Tap` provider — 44 roast files, 6 real bugs behind it

`use Test::Tap` is a native no-op in `src/runtime/runtime_module.rs`:

```rust
} else if module == "Test::Tap" {
    // Handle Test::Tap as built-in
    Ok(())
}
```

so `roast/packages/Test-Helpers/lib/Test/Tap.rakumod` is never loaded and
`tap-ok` is answered by `src/runtime/test_functions/tap_ok.rs`. This is a
rung-3 native provider in the sense of `docs/BATTERIES.md`, and the same kind of
thing `Pod::To::Text` and the `monitor` declarator were retired for.

It is also a hard blocker for `todo/tickets/vendor-real-test-module.md` step 3.
Under `MUTSU_REAL_TEST=1` the 44 whitelisted files that `use Test::Tap` keep two
counters — the module's `plan`/`ok` and the native `tap-ok` — so each reports
`# You planned N tests, but ran 0` at END even though every `ok` line printed
correctly. That is the `S17-supply` cluster, the single largest group in the 343
roast regressions measured on 2026-08-02.

Note this is **not** the `user_test_decl_beats_native` guard: `tap-ok` is
already in `TEST_MODULE_EXPORTS`, so the guard *is* consulted and declines only
because there is no declaration to find — the module was never loaded. Widening
the guard (`todo/tickets/retire-native-test-util-overrides.md`) does not touch
this.

## What deleting the arm buys, and what it costs

Deleting the arm is enough for the module to load: all 44 files resolve
`Test::Tap` through the dist directory their `use lib $*PROGRAM.parent(2).add(
"packages/Test-Helpers")` points at (mutsu reads `META6.json` `provides`), and
`roast/S17-supply/elems.t` then passes under *both* providers.

**6 of the 44 regress under the default native provider**, because the real
`tap-ok` asserts with the real `is-deeply` where mutsu's native one was lenient.
All six pass under `raku`, so all six are real mutsu bugs the provider was
hiding. They fall into two causes:

| cause | files | shape |
| --- | --- | --- |
| a tap callback's `@res.push($_)` collects nothing when the emit runs on a timer/scheduler thread — `todo/tickets/array-push-from-a-timer-thread-is-lost.md` | `S17-supply/classify.t`, `categorize.t`, `interval.t`, `merge.t`, `reduce.t` | `# expected: [0, 1, 2, 3, 4]` / `# got: []` |
| `Supply.rotor` emits `List`s where rakudo emits `Array`s | `S17-supply/rotor.t` | `# expected: [[1, 2, 3], …]` / `# got: [(1, 2, 3), …]` |

The first was *guessed* to be the same cross-thread lexical family as
`t/subtest-threaded-pass-count.t` (fixed by
`news/2026-08/a-callee-parameter-is-not-a-shared-variable.md`). **It is not** —
re-measured with that fix in, the same 6 files fail. It is its own bug, and it
needs no `Test`, no module and no routine:

```raku
my @res;
my $done;
Supply.interval(0.1).head(3).tap({ @res.push($_) }, :done({ $done = True }));
for ^40 { last if $done; sleep .1 }
say "done=$done res=@res[]";
```

```
$ raku                     $ mutsu
done=True res=0 1 2        done=True res=
```

Note what that isolates: the `:done` callback's write to the **scalar** `$done`
crosses the timer thread correctly, and only the **array** pushes are lost. So
this is the `@`-aggregate lane of the shared store (the `__mutsu_atomic_arr::`
CAS copies), not the scalar lane — a different mechanism from the parameter fix
above, and adjacent to `354cd623f` ("an array alias survives a thread having
run") and `t/hyper-array-mutators.t`.

## Order of work

1. Fix the two causes above (they are general bugs; pin each in `t/`).
2. Delete the `module == "Test::Tap"` arm.
3. Decide what to do with `src/runtime/test_functions/tap_ok.rs` and the
   `"tap-ok"` entries in `TEST_MODULE_EXPORTS` / `is_test_function_name` /
   `compile_consts.rs` — with the module loading, the native handler is only
   reachable when it is *not* loaded, which is no longer a case roast has.

Measure with the 44 files directly:

```bash
grep -rl 'use Test::Tap' roast/ | grep '\.t$' | sort > tmp/taptests.txt
comm -12 tmp/taptests.txt <(sort roast-whitelist.txt) > tmp/taptests-wl.txt
MUTSU_BIN=target/release/mutsu prove -j6 -e 'scripts/run-roast-test.sh' $(cat tmp/taptests-wl.txt)
```

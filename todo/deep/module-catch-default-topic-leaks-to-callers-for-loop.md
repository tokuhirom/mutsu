# A `CATCH { default { } }` inside a loaded module's routine leaks its topic into the caller's `for` loop

Found while resuming `todo/tickets/vendor-real-test-module.md` (2026-08-15),
investigating why `roast/S03-operators/range.t` fails under
`MUTSU_REAL_TEST=1`. Not a `Test`-shape problem and not related to
`X::Worry::Precedence::Range` (that class works correctly end to end) — it is
a general topic (`$_`) scoping bug that only shows up once the call goes
through a *loaded module's* own routine.

## Minimal repro

```raku
use Test;
plan 2;
for '..', '^..' {
    throws-like "\{ use fatal; |4$_ 5 }", X::Worry::Precedence::Range, "warns 1";
    say "  topic after call 1 = [$_.substr(0, 40)]";
}
```

Run with `MUTSU_REAL_TEST=1` (so `use Test;` loads the real, vendored
`modules/Rakudo-Core/lib/Test.rakumod` instead of mutsu's native `Test`
provider):

```
$ MUTSU_REAL_TEST=1 target/debug/mutsu repro.raku
ok 1 - warns 1
  topic after call 1 = [To apply a Slip flattener to a range, pa]
ok 2 - warns 1
  topic after call 1 = [To apply a Slip flattener to a range, pa]
```

Both assertions correctly PASS (`throws-like` correctly identifies the
exception as `X::Worry::Precedence::Range`), but after the call returns, the
enclosing `for` loop's own topic `$_` has been permanently overwritten with
the *caught exception's message text* — instead of remaining `'..'` /
`'^..'`. `raku` (via the same unmodified `Test.rakumod`) leaves `$_` correctly
at the loop's own value throughout.

## Why it matters

In `roast/S03-operators/range.t`'s actual `for @opvariants { ... }` loop
(lines 269-273), the loop body builds each of its four assertions' source
strings by interpolating `$_` (`"\{ use fatal; |4$_ 5 }"`). Once the first
`throws-like` call clobbers `$_`, every subsequent statement in that same
iteration — and every subsequent loop iteration, since the corruption
persists — builds garbage source strings (the *previous* exception's message
concatenated into what should have been `..`/`^..`/etc.), which then fail to
parse and report the generic `X::Syntax::Confused` instead of ever reaching
the assertion under test. This is why the `vendor-real-test-module.md`
campaign's residue classified this file under "Got: X::Syntax::Confused" —
the class itself is a red herring; the actual bug is the topic leak that
corrupted the source text several statements earlier.

## What was ruled out

The natural hypothesis is that `CATCH { default { $_ = <exception> ... } }`
simply doesn't restore the topic when the handler exits. That mechanism is
in fact implemented correctly and was verified directly:

- `src/vm/vm_try_catch_ops.rs`'s `run_range`-based CATCH-region handler
  (around line 450) explicitly saves `$_` (`saved_topic`) before binding it to
  the caught exception and restores it (or removes the key if it was unset)
  on every exit path — the `Ok(())`, `.resume`, and `succeed`-from-`when`
  branches all restore it.
- A bare `try { die; CATCH { default { ... } } }` inside a `for` loop (no
  module involved) does **not** leak: `$_` is correctly restored after the
  `try` block exits, confirmed directly.
- A hand-written sub with the exact same shape as `Test.rakumod`'s
  `throws-like` — `subtest { ...; EVAL $code, context => CALLER::; CATCH {
  default { pass $msg; ... ok $type_ok, ...; } } }`, called from inside a bare
  `for` loop, using the real `subtest`/`pass`/`ok` from `use Test;` — does
  **not** leak either, even copied nearly verbatim from the vendored source
  and called twice per iteration.

So the leak is not in the CATCH/topicalization mechanism itself, nor merely
in going through nested sub/closure calls (`subtest` invoking its `&subtests`
block argument). It requires the actual `throws-like` call to go through the
real, *loaded module's* compiled routine — i.e. crossing into
`modules/Rakudo-Core/lib/Test.rakumod`'s own compunit — not an equivalent sub
declared inline in the same file as the caller. This points at the same
family of bugs already on file about module/compunit-scoped state leaking
across the module boundary by bare name:

- `todo/deep/shared-store-bare-name-collision-across-unrelated-frames.md` —
  once a thread has been spawned, a plain lexical write goes through the
  process-global `shared_vars` map keyed by bare name, so two unrelated
  frames using the same variable name collide. This repro does not spawn any
  thread, so if the same map is involved, something else about crossing a
  module boundary must also route through it (or a sibling mechanism does).
- `todo/tickets/module-file-scope-array-and-hash-still-share-the-caller.md` —
  `Test.rakumod`'s own `@`/`%` file-scope lexicals (`@vars`, etc.) are known
  to still share an env key with the loading scope. The topic key `_` may be
  suffering the same category of collision when *reached from inside* a
  loaded module's routine, rather than only when the module's own file-scope
  container is read.

## Next steps for whoever picks this up

1. Reproduce with `MUTSU_DEBUG_CLOBBER`-style instrumentation on `Env::insert`
   / `insert_sym` for the key `"_"` specifically (see the shared-store ticket
   above for the pattern), filtered to when the write happens from inside a
   compiled module routine's frame, to catch the exact write that survives
   past the CATCH region's restore.
2. Check whether the write is happening *after* `vm_try_catch_ops.rs`'s
   restore point at all — i.e. whether something written *inside* the CATCH
   handler (a call to `pass`/`ok`/`_diag`, all real module routines with their
   own control flow) reaches back out to the outer frame's `_` through a
   different route than the CATCH region's own env, bypassing the
   save/restore pair entirely (e.g. if the module's routines resolve `$_`
   through the bare-name shared-store lane rather than the current frame's
   `env`).
3. Confirm whether this reproduces with *any* module-provided sub containing
   `CATCH { default { ... } }`, or whether it is specific to `Test.rakumod` /
   `throws-like`'s particular shape (calling `pass`, `ok`, and a nested
   `with ... -> $nested-ex { }` inside the handler).

## What it blocks

`roast/S03-operators/range.t` under `MUTSU_REAL_TEST=1`
(`todo/tickets/vendor-real-test-module.md`); likely other files in that
campaign's residue that use `throws-like`/`eval-lives-ok` inside a bare `for`
loop with an interpolated topic, since the same mechanism would corrupt any
of them the same way.

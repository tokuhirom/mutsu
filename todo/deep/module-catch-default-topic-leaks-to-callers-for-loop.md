# A `CATCH { default { } }` inside a loaded module's routine leaks its topic into the caller's `for` loop

Found while resuming `todo/deep/vendor-real-test-module.md` (2026-08-15),
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

## Root cause found (2026-08-16), not yet fixed

Located the exact write site via a `rust-gdb` breakpoint sweep on `Env::insert`
for the 1-char key `"_"`: `src/vm/vm_call_named_inner.rs:238-244`

```rust
// Raku: routines get their own $_ initialized to (Any).
if cf.code.is_routine && !cf.param_defs.iter().any(|pd| pd.name == "_") {
    self.env_mut().insert("_".to_string(), Value::package(Symbol::intern("Any")));
}
```

This write is correct Raku semantics in isolation (a routine call gets a fresh
`$_`), but it hits the interpreter's single "current env" field with no
per-call push/pop of a separate topic scope — a backtrace sweep showed
`_init_vars`, `plan`, `throws-like`, `subtest`, `_diag`, `pass`, `ok`,
`proclaim` all sharing the *same* env-field address across nested calls.
Ordinary user code doesn't see this because a `for` loop's topic reads/writes
normally go through the fast `locals` array (reset-then-discarded per call,
harmless) — but `vm_try_catch_ops.rs`'s CATCH/`default{}` topicalization
deliberately bypasses `locals` and does a raw `self.env().get("_")` /
`insert("_", …)` save-restore, a **flat one-level** save/restore, not a real
stack. Once several nested routine entries (each resetting `_`→`Any`)
interleave with the CATCH's own bind-to-exception and restore — inside
`throws-like` → `subtest` → (EVAL) → `CATCH default { pass; ok; _diag }` — the
*last* write to survive the unwind is the exception's message text, not the
loop's item. Confirmed a plain user-defined sub call inside a `for` loop does
**not** leak on its own; the trigger needs the CATCH/`default{}` +
multi-level-nested-call combination specifically. So this is a narrower
instance of the dual-store (`locals` vs `env`) debt CLAUDE.md already flags,
not the bare-name shared-store lane hypothesized above (no thread is
spawned in the repro).

**This is not a one-line patch.** A correct fix needs topic save/restore to
nest properly across arbitrary call depth — e.g. an explicit topic stack, or
migrating the CATCH region's topic handling onto the same `locals`-based fast
path that ordinary routine calls already use safely — which is a design-level
change to how `$_` is threaded through nested frames, not a targeted patch to
`vm_call_named_inner.rs` or `vm_try_catch_ops.rs` alone. Stays a `todo/deep/`
item pending a design pass.

Other roast files combining a `for` loop with `throws-like`/`eval-lives-ok`/
`eval-dies-ok` (`S32-exceptions/misc.t`, `S03-operators/ternary.t`,
`integration/error-reporting.t`, others) are plausible candidates for the same
mechanism but were not individually confirmed.

## Next steps for whoever picks this up

1. Design a real topic stack (or equivalent) so nested routine-entry resets of
   `_` cannot clobber an outer CATCH region's saved value — see "Root cause
   found" above for the exact write site and the mechanism.
2. Confirm whether this reproduces with *any* module-provided sub containing
   `CATCH { default { ... } }`, or whether it is specific to `Test.rakumod` /
   `throws-like`'s particular shape (calling `pass`, `ok`, and a nested
   `with ... -> $nested-ex { }` inside the handler).

## What it blocks

`roast/S03-operators/range.t` under `MUTSU_REAL_TEST=1`
(`todo/deep/vendor-real-test-module.md`); likely other files in that
campaign's residue that use `throws-like`/`eval-lives-ok` inside a bare `for`
loop with an interpolated topic, since the same mechanism would corrupt any
of them the same way.

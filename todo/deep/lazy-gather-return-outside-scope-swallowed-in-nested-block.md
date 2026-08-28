# A `return`-outside-scope raised while forcing a lazy `gather` is swallowed (or misreported) inside a nested block

## Root cause (as far as diagnosed)

`gather { return }` builds a lazy sequence; when the resulting `Seq` is
later forced (e.g. by `.Str`/`~`), the `return` fires *then*, dynamically,
long after `f()`'s own call frame is gone. Rakudo converts this into a
catchable `X::ControlFlow::Return` ("Attempt to return outside of
immediately-enclosing Routine ...").

mutsu gets this right at the **top level** and inside a **`try { }`**
block: both raise a proper `X::ControlFlow::Return` that a `CATCH`/`$!`
can see. But the moment the force happens inside an **ordinary nested
block** — a bare `{ ... }`, or a `Callable` invoked through a plain
user-defined sub — the signal is handled wrong in two different ways
depending on the exact nesting:

1. **Bare block at the mainline** (`tmp` repro below): the raw internal
   message reaches the CLI uncaught (`CATCH { default { ... } }` inside
   the very same block never fires), and — worse — the message printed
   is the *non-EVAL* wording ("outside of immediately-enclosing Routine")
   even though a `Comp`-context matching wording exists for the EVAL
   case; the `CATCH` should have caught this before it ever got to CLI
   rendering.
2. **Closure invoked via a plain user sub** (`call-it(&code) { code() }`,
   or the shape `Test.rakumod`'s real `subtest(&subtests) { subtests();
   CATCH {...} }` uses): the signal is silently **swallowed** — neither
   the inner `CATCH` fires, nor does anything propagate to top level, nor
   does the script exit non-zero. The script just continues as if the
   `EVAL` returned normally, but every statement *after* the `EVAL` in
   that block is also skipped (as if a real `return`/unwind
   happened) — i.e. the signal partially behaves like a working control
   transfer, just to the wrong place, with no exception delivered to any
   `CATCH` along the way.

This was found while working the `MUTSU_REAL_TEST=1` residue for
`roast/S32-exceptions/misc2.t` (see `todo/deep/vendor-real-test-module.md`).
Line 368 there is:

```raku
throws-like 'my sub f() { gather { return } }; ~f()', X::ControlFlow::Return;
```

Under the real vendored `Test.rakumod`, `throws-like` builds a `subtest {
plan 2; ...; EVAL $code, context => $caller-context; ...; CATCH { default
{ pass $msg; ... } } }` closure and hands it to the real `subtest(&code)`
sub (`modules/Rakudo-Core/lib/Test.rakumod` around line 420), which just
calls `subtests()` directly. That is exactly case 2 above: the CATCH
inside the closure never fires, the subtest's own `plan 2` never gets its
`ok`/`not ok` lines, and — because `subtest()`'s wrapper never sees the
closure "return" (Rakudo's `proclaim`/`done-testing` machinery is skipped
entirely) — **every `throws-like` call after this one in the same file
is misattributed as nested inside this subtest's TAP scope**, producing
`# You planned 2 tests, but ran 40` far downstream and aborting the whole
run. This is NOT specific to `Test.rakumod`; case 2 reproduces with a
two-line user sub and no `use Test` at all (see repro).

Under mutsu's OWN native `throws-like` (`src/runtime/test_functions/`),
this exact roast line passes — the native provider's own `lives-ok`/
`throws-like` implementation has its own special-case handling for a live
non-local-control signal (`is_live_nonlocal_control` in
`src/runtime/test_functions/eval_exception.rs`) that apparently does not
hit this bug, or masks it. So `make roast` under the whitelisted (native)
provider is unaffected; this is purely a `MUTSU_REAL_TEST=1` residue
finding, but the underlying mechanism (a lazy-forced Return escaping a
nested block's CATCH) is a real, general correctness bug independent of
Test.

## Minimal repros

Case 1 (bare block, message reaches CLI uncaught, `CATCH` inside the same
block never fires):

```raku
{
    EVAL 'my sub f() { gather { return } }; ~f()';
    say "unreached: eval did not throw";
    CATCH {
        default {
            say "caught: ", $_.^name;
        }
    }
}
say "after";
```

mutsu (release): prints `Attempt to return outside of immediately-enclosing
Routine (...)` and appears to abort with a non-zero exit — nothing after
that line is printed, not even `after`.
raku: prints `caught: X::ControlFlow::Return` then `after`.

Case 2 (closure through a plain user sub, signal silently swallowed,
downstream statements in the SAME block also skipped):

```raku
sub call-it(&code) {
    code();
}
call-it({
    EVAL 'my sub f() { gather { return } }; ~f()';
    say "unreached: eval did not throw";
    CATCH {
        default {
            say "caught: ", $_.^name;
        }
    }
});
say "after";
```

mutsu (release): prints only `after` — neither `say` inside the closure
ever runs, no error, exit 0.
raku: prints `caught: X::ControlFlow::Return` then `after`.

For contrast, both of these work correctly in mutsu today (already
verified, not part of this finding):

```raku
try { EVAL('my sub f() { gather { return } }; ~f()') };
say $!.^name;   # X::ControlFlow::Return, matches raku
```
```raku
my sub f() { gather { return } };
try { ~f() };
say $!.^name;   # X::ControlFlow::Return, matches raku
```

## Why this is `deep`, not a `tickets` slice

The signal clearly propagates *somewhere* wrong rather than simply being
dropped — case 2's "rest of the block after EVAL is skipped, but nothing
downstream ever sees it either" behavior suggests the lazy-gather-force's
`X::ControlFlow::Return` conversion path (see `resolution_lazy.rs`,
`vm_exec_dispatch.rs` around the `X::ControlFlow::Return` /
`out-of-dynamic-scope` comments, and `runtime/mod.rs`'s "uncaught-CX::Return
-> X::ControlFlow::Return conversion in `run_inner`") is racing or
mismatching against the CATCH-region bytecode boundaries
(`vm_try_catch_ops.rs`) specifically when the force happens through an
*extra* frame boundary (a real Callable call, or a nested nameless block)
that the conversion logic does not expect. Understanding exactly which
boundary swallows it (block-scope exit vs. routine-call return vs. the
lazy-list coroutine's own frame teardown) needs a design pass across
`resolution_lazy.rs`, `vm_control_ops.rs`/`vm_try_catch_ops.rs`, and the
"uncaught-CX::Return" conversion site in `runtime/mod.rs`, not a
single-file fix.

## Affected files (starting points for the investigation)

- `src/runtime/resolution_lazy.rs` (the doc comment there already claims
  this exact conversion — worth checking why it does not fire for cases 1/2)
- `src/vm/vm_exec_dispatch.rs` (the `Return`/`ReturnFromNonRoutine` /
  `X::ControlFlow::Return` arms)
- `src/vm/vm_try_catch_ops.rs` (`dispatch_to_catch_handler` and the
  CATCH-region bytecode boundaries)
- `src/runtime/mod.rs` (`run_inner`'s "uncaught-CX::Return ->
  X::ControlFlow::Return conversion", ~line 3025)
- `src/runtime/test_functions/eval_exception.rs` (`is_live_nonlocal_control`
  — the native `throws-like`/`lives-ok` special-case that apparently avoids
  this bug; understanding why it avoids it may point at the fix)

## Impact

Blocks `roast/S32-exceptions/misc2.t` from passing cleanly under
`MUTSU_REAL_TEST=1` (aborts partway through with "You planned 2 tests,
but ran 40" once the earlier-in-file `X::ControlFlow::Return` subtest
swallows the signal and desynchronizes every subsequent subtest's TAP
nesting). Does not affect the native `Test` provider (`make roast`
remains green for this file). Likely affects any real-world Raku script
that forces a lazy `gather` containing a scope-escaped `return` from
inside a nested block or a Callable passed to a plain sub — a general
correctness gap, not a roast-only curiosity.

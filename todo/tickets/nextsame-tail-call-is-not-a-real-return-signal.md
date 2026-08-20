# `nextsame`/`nextwith`'s tail-call unwind is not a real `CX::Return`, so it stops at the nearest dynamic frame

**Status: ready for direct implementation.** The fix is verified (see
"Verified fix" below) and is a mechanical one-liner at each of twelve sites in
one file. Not implemented here because the investigation that produced it was a
design-only pass.

Originally filed as `todo/tickets/lastcall-in-wrapper-nextsame-swallows-output.md`
(found while fixing `lastcall-in-wrapper-callsame-dies` — see
`news/2026-08/lastcall-in-wrapper-callsame-dies.md`), then reclassified to
`todo/deep/nextsame-in-wrap-closure-lexical-return-target.md` on the theory that
mutsu had no lexical closure-to-Routine binding for `return` at all. A
re-verification pass on 2026-08-20 (mutsu @ `227e38e4f`, raku v2026.06) showed
that theory was wrong on both counts, and split the finding in two:

- **This ticket** — `nextsame`/`nextwith` build their unwind signal by hand and
  omit `control: Control::Return`, so the *existing* lexical re-target
  machinery declines to look at it. Small, verified, self-contained.
- **[ADR-0050](../../docs/adr/0050-block-routine-ness-is-a-definition-site-property.md)**
  — a Block's routine-ness is re-derived from the dynamic call stack when its
  body is re-compiled by the block-value carrier, discarding the correct
  definition-site classification. That is the architectural half, and it is
  independent of this ticket in both directions.

The old file's claim that mutsu lacks a lexical return target is **stale**:
`return` in the identical position already targets the lexically enclosing
Routine correctly (probe `tmp/v5` below). So is the old file's variant 2/3
evidence — raku v2026.06 prints `wrap` *and* `Nil` for the `lastcall; nextsame`
shapes, which mutsu matches exactly, and its item 3 ("`lastcall`'s exact
interaction with `nextsame` needs its own investigation") is a non-issue.

## Root cause

`Interpreter::dispatch_next_candidate` (`src/runtime/builtins_dispatch_next.rs:488`)
takes a `tail_call: bool` — true for `nextsame`/`nextwith`, false for
`callsame`/`callwith` — and every one of its twelve `tail_call` legs raises the
unwind by hand:

```rust
return Err(RuntimeError {
    return_value: Some(result),
    ..RuntimeError::new(""),
});
```

Sites (line numbers @ `227e38e4f`): 526, 534, 647, 710, 995, 1060, 1141, 1154,
1196, 1213, 1226, 1241.

That is **not** what a `return` raises. `RuntimeError::return_signal`
(`src/value/error.rs:636-643`) additionally sets `control: Some(Control::Return)`
and `message: "CX::Return"`. `control` is what `RuntimeError::is_return()`
(`src/value/error.rs:290`) reads — and `is_return()` is the gate on the arm
that gives a non-Routine block's return its *lexical* target:

```rust
// src/runtime/resolution_call_sub.rs:1060-1081
let is_non_routine =
    data.is_bare_block || data.compiled_code.as_ref().is_some_and(|cc| !cc.is_routine);
if is_non_routine
    && let Err(ref e) = result
    && e.is_return()          // <-- nextsame's hand-built signal fails here
{
    let has_target = e.return_target_callable_id().is_some()
        || data.env.contains_key("__mutsu_callable_id");
    if has_target { /* stamp the lexically enclosing routine's id, re-raise */ }
}
```

So a `nextsame` unwind is a *targetless* signal that never gets stamped, and
the first frame matching `Err(e) if e.return_value.is_some()` swallows it —
the wrapped method, rather than the Routine the wrapper block was written
inside. Every decline-if-not-my-target site
(`vm_call_named_inner.rs:326-343`, `vm_method_dispatch.rs:743-751` / `:1726-1735`)
only declines when the signal *has* a target, so a targetless one is absorbed
unconditionally.

## Repro

```raku
# tmp/v1.p6
sub run1() {
    class C1 { method m() { say "orig"; "o" } }
    C1.^lookup('m').wrap(-> |c { say "wrap"; nextsame; say "unreached" });
    say C1.new.m;
}
run1();
say "after";
```

```
raku            mutsu (before)
wrap            wrap
orig            orig
after           o          <-- `say C1.new.m` should never run
                after
```

raku's `nextsame` tail-calls the original `m` (printing `orig`, yielding `"o"`)
and then returns that value **from `run1`** — the Routine the wrapper block is
lexically nested inside — abandoning `say C1.new.m` mid-statement. `run1()` is
in sink context, so the `"o"` is simply discarded.

Two companion probes fix the boundaries of the fix:

- `tmp/v8` — the same wrapper with `my $x = nextsame` (non-tail position):
  raku still unwinds (`wrap / orig / after`), so this is not about syntactic
  tail position. mutsu (before) printed `wrap / orig / o / run8-end / after`.
- `tmp/v14` — the same wrapper written as `sub (|c) { ... nextsame ... }`:
  the wrapper *is* a Routine, so `nextsame` returns from the wrapper itself and
  `say C.new.m` legitimately prints `o`. raku and mutsu already agree here, and
  **must keep agreeing** — this is the case a naive "always unwind further"
  fix would break.

(This also refutes the old file's item-3 hypothesis that the target search is
about where the code was textually written rather than whether the invoked
callable is a Routine: it is exactly the latter.)

## Verified fix

Replace each of the twelve hand-built signals with the real constructor:

```rust
return Err(RuntimeError::return_signal(result));
```

Measured on 2026-08-20 with that change applied locally:

- `tmp/v1` → `wrap / orig / after` — matches raku.
- `tmp/v8` → `wrap / orig / after` — matches raku.
- `tmp/v14` → unchanged `wrap / orig / o / run14-end / after` — still matches raku.
- Green: the whole wrap/`callsame` corner of `t/` — `wrap.t`,
  `lastcall-then-nextsame.t`, `lastcall-in-wrapper-callsame-dies.t`,
  `nextsame-role-mixin.t`, `nextsame-rw-redispatch.t`,
  `method-wrap-callsame-order.t`, `wrap-mid-mro-callsame.t`,
  `wrap-recursive-redispatch.t`, `wrap-chain-foreign-wrapper-not-shadowed.t`,
  `wrap-multi-candidate-scope.t`, `build-callsame-nil.t`,
  `new-callsame-native-mu-fallback.t`,
  `gist-str-raku-callsame-native-mu-fallback.t`,
  `callsame-punned-role-and-hyper-infix-sub.t` (14 files, 69 tests).
- Green: `roast/S06-advanced/wrap.t`, `roast/S04-statements/return.t`,
  `roast/S06-advanced/return.t`, `roast/S12-methods/multi.t` (257 tests).

Note the fix only bites through the *interpreter* twin
(`resolution_call_sub.rs:1062`, which is `is_return()`-gated). The VM twin
(`vm_closure_dispatch.rs:863`) is gated only on `e.return_value.is_some()`, so
it would have stamped a targetless signal already — an asymmetry between the
two twins worth collapsing while in the area, though it is not what this
ticket's repro exercises.

## What to add

- The change above at all twelve `tail_call` legs of
  `src/runtime/builtins_dispatch_next.rs`.
- `t/nextsame-in-block-wrapper-returns-lexically.t` pinning the three probes
  (`tmp/v1`, `tmp/v8`, `tmp/v14`) — the third is the anti-regression half.
- Run the full `t/` wrap/`callsame` corner and the four roast files listed
  above; let CI cover the rest.

## Known residue (NOT this ticket)

The no-lexically-enclosing-Routine case still diverges after this fix:

```raku
class C7 { method m() { say "orig"; "o" } }
C7.^lookup('m').wrap(-> |c { say "wrap"; nextsame; say "unreached" });
say C7.new.m;
# raku:  wrap, then dies "Attempt to return outside of any Routine"
# mutsu: wrap / orig / o / after
```

That is ADR-0050's subject (the wrapper block is re-compiled as a Routine by
the block-value carrier, so it becomes its own return boundary and absorbs the
signal). The same divergence appears with a plain `return` in place of
`nextsame`, which is why it is not this ticket.

# `nextsame`/`return` inside a `.wrap()` closure targets the LEXICALLY enclosing Routine, not the nearest dynamic one

Originally filed as `todo/tickets/lastcall-in-wrapper-nextsame-swallows-output.md`
(found while fixing `lastcall-in-wrapper-callsame-dies`, now closed — see
`news/2026-08/lastcall-in-wrapper-callsame-dies.md`). Reclassified to `todo/deep/`
after a raku-verification pass (2026-08-17) showed the divergence is broader than
the original probe's `lastcall`-then-`nextsame` scope: it is a missing piece of
mutsu's closure/return architecture, not a one-off dispatch-chain bug.

## The core semantics raku has and mutsu doesn't

In raku, `return` (and the `nextsame`/`nextcallee`/`callsame` family, which are
built on the same "return from a Routine" primitive when used as a bare tail
statement) targets the Routine (`sub`/`method`) the executing code is **lexically**
nested inside — determined at closure-creation time by the block's defining
scope, NOT by which Routine frame happens to be nearest on the dynamic call
stack when the block is later invoked. A bare `Block` (`-> |c { ... }`, or a
`method`/`sub` literal passed as a value) is not itself a Routine boundary for
this purpose; a `return`/`nextsame` written inside one keeps searching lexically
outward until it finds an actual enclosing `sub`/`method` — which, for a
`.wrap()` closure written inside another `sub`, is that OUTER sub, not the
method being wrapped and not any dispatch-machinery frame.

mutsu's tail-call encoding for `nextsame` (`RuntimeError { return_value:
Some(Value::NIL) }`, unwinding to "the nearest routine boundary") is a purely
DYNAMIC/stack-based mechanism: it stops unwinding at whatever routine frame it
encounters first while propagating up the call stack, with no notion of which
Routine the closure was lexically defined inside. This is a genuine architecture
gap (closures don't carry a "lexically enclosing Routine" target for return
purposes), not a narrow dispatch-chain quirk.

## Verified behavior (raku v2026.06), three variants

```raku
sub run1() {
    class C1 { method m() { say "orig"; "o" } }
    C1.^lookup('m').wrap(-> |c { say "wrap"; nextsame; say "unreached" });
    say C1.new.m;
}
run1();
```
```
wrap
orig
```
No `o`/`Nil` is ever printed by `say C1.new.m` — `nextsame` (even WITHOUT
`lastcall`) tail-calls the original `m` (which prints "orig" and returns "o"),
but then propagates that "return" all the way out through `run1()`'s frame
(since the wrap closure is lexically inside `run1`), so `run1()` itself returns
early with value `"o"` — abandoning the `say C1.new.m` statement mid-evaluation.
`run1();` is a void-context statement, so the returned `"o"` is simply discarded.

```raku
sub run2() {
    class C2 { method m() { say "orig"; "o" } }
    my &wrapper = sub (|c) { say "wrap"; lastcall; nextsame; say "unreached" };
    C2.^lookup('m').wrap(&wrapper);
    say C2.new.m;
}
run2();
```
```
wrap
```
With `lastcall` set, `nextsame` does NOT reach the original `m` at all — no
"orig" is printed, unlike variant 1. This is the original ticket's scenario
(also true for a `sub (|c) {...}` wrapper, not just a bare `-> |c {...}` block —
so it's not specific to Block-vs-Sub either). `lastcall`'s exact interaction
with `nextsame`'s "is there a next candidate" check needs its own investigation
— it looks like `lastcall` makes `nextsame` treat the call chain as already
exhausted, rather than "one real candidate left, tail-call it".

```raku
sub run3() {
    class C3 { method m() { say "orig"; "o" } }
    C3.^lookup('m').wrap(method (|c) { say "wrap"; lastcall; nextsame; say "unreached" });
    say C3.new.m;
}
run3();
```
```
wrap
```
Same as variant 2 — a `method (|c) {...}` wrapper (its own Routine, unlike a
bare Block) behaves identically to the `sub (|c) {...}` case here, so
"the wrapper is its own Routine" does NOT change the lexical-target search
outcome in this specific lastcall+nextsame combination (both still unwind past
`m` and past the wrapper itself to `run3`'s frame). This suggests the *lexical
enclosing Routine* target-finding is about where the code implementing
`nextsame`'s return-unwind was WRITTEN (inside `run3`'s body, textually), not
about whether the immediately-invoked callable happens to be a Routine.

At the top level of a script (`-e`, no enclosing `sub` at all), raku instead
dies with `Attempt to return outside of any Routine` at the `nextsame` — there
is no lexically enclosing Routine to target. Not reproduced by mutsu either
(mutsu returns `Nil` there too, same as the wrapped case).

## What's needed for a real fix

1. Track, per closure/Block at creation time, which Routine (if any) lexically
   encloses it — a compile-time/AST-scope property, not a runtime dispatch
   concept. This likely means threading a "lexical routine id" through block/
   closure compilation (`compiler/`) and storing it on the closure Value.
2. Change `return`/`nextsame`'s unwind mechanism to carry that target Routine
   id and unwind PAST any intervening routine frames (e.g. the wrapped method
   `m`, or the wrapper itself if it's a `method`/`sub`) until it reaches a
   frame matching that id — not just "the nearest routine frame encountered."
   This is a different mechanism from the current `RuntimeError{return_value}`
   dynamic-unwind-to-nearest-frame approach.
3. Separately investigate `lastcall`'s exact effect on whether `nextsame`
   still dispatches to the "real" next candidate (variant 1 vs variant 2/3
   above suggests `lastcall` changes this, independent of the lexical-target
   issue).
4. The top-level (`-e`, no enclosing routine) "Attempt to return outside of
   any Routine" error is a smaller, more separable piece: when the lexical
   target search finds no enclosing Routine at all, that should be a
   catchable die, not a silent `Nil`.

## Scope note

Not chased further here — items 1-2 are a real architectural gap (lexical
closure-to-Routine binding for return/nextsame targeting) that needs its own
design pass before touching code, per the project's usual discipline for
dispatch-chain semantics. `lastcall`-then-`nextsame` in a plain (non-wrapper)
multi context already matches raku and is pinned by `t/lastcall-then-nextsame.t`
— this is specifically about the closure-lexical-scoping interaction, which is
broader than "wrapper" cases: any nested Block containing a bare `return`-like
statement plausibly has the same gap, independent of `.wrap()`.

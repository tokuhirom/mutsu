# `EVAL ..., context => $frame`'s `return` now targets the context frame, not just the EVAL caller

`EVAL $code, context => $ctx` compiles `$code` as if it stood at `$ctx`'s
frame. mutsu already took the *package* from `$ctx`
(`news/2026-08/eval-context-argument.md`), but everything about control
flow — including where a `return` in the snippet goes — was still resolved
against whichever frame happened to call `EVAL`. Measured against `raku`:
when `$ctx` names a routine still live on the dynamic call stack, `return`
must unwind *past* the `EVAL` caller to that specific frame, not stop at the
first routine boundary it reaches. This is exactly the shape rakudo's real
`Test.rakumod` writes (`my $caller-context = $*THROWS-LIKE-CONTEXT //
CALLER::; subtest { ... EVAL $code, context => $caller-context; ... }`), so
every `throws-like '<code that returns>', X::ControlFlow::Return` assertion
reported the code as not having died.

The full design is [ADR-0037](../../docs/adr/0037-eval-context-frame-owns-the-return-target.md),
landed across five slices. The first three (routine frames sound on every
dispatch path, `CALLER::`/`CALLERS::` stamping the enclosing routine's
control-flow identity alongside its package, and `context` classifying the
snippet's `return` as mainline/live/dead) landed 2026-08-20/21. This entry
retires the origin ticket after the final two:

## Slice 4: targeting the live frame specifically

Classifying a context as "live" is not enough on its own — mutsu needed to
target *that* frame, not just treat the snippet as "inside some routine" (which
the first routine boundary the signal reaches would then catch). When a
context's routine is live, `builtin_eval` now resolves that routine's
registration clone id — the same identity `RuntimeError::return_target_callable_id`
already compares against, used for years to route a bare block's `return` to
its lexically enclosing routine. `compile_block_value_opts` bakes the id onto
the EVAL unit's own compiled chunk (a `CompiledCode` field, not a growth to
the payload-free `OpCode::Return` — the opcode size guard stays intact), and
`OpCode::Return`'s exec arm stamps it onto the raised signal. Every routine
boundary already knew how to decline a signal aimed at someone else; this
slice's job was purely getting the right id onto the signal in the first
place.

Verifying it against the ADR's own two-deep repro surfaced a second,
independent bug: mutsu's two "light" call dispatch paths (for
mandatory-positional-only and named-only signatures) caught *any* return
signal unconditionally, never checking whether it was actually meant for
them. A light-dispatched routine sitting between the `EVAL` call and its
live-context target swallowed the signal itself instead of letting it pass
through — the exact shape of the ADR's own repro, since a one-positional-arg
`sub thrower($code)` is light-dispatched. Fixed by adding the same
decline-if-not-my-target check the heavier dispatch paths already had, gated
so the lookup only runs when a signal actually carries a target (the
overwhelmingly common untargeted case pays nothing extra). This was a
genuine, general-purpose fix, not an EVAL-specific one: any non-local return
aimed at a specific routine — including the pre-existing bare-block-return
case — could already have been mis-caught by an intervening light-dispatched
frame; ADR-0037's own repro is simply the first thing that surfaced it.

## Slice 5: residue and end-to-end

Swept the three items the ADR named by name. `CALLERS::` (plural) is stamped
identically to `CALLER::` at the same site, so it already worked with no
further change — verified and pinned. `$*THROWS-LIKE-CONTEXT`
(`Test::Util`'s `no-fatal-throws-like`) stores a captured `CALLER::` in a
dynamic variable that `Test.rakumod`'s `throws-like` reads back several
frames deeper inside its own `subtest { ... }` — exactly the
capture-now/use-later shape the ADR's design is built around — and the
targeting survives being threaded through that indirection. `EVALFILE` has no
`context` parameter at all (per `raku-doc`), so it was never in scope; its
plain uncontextualized `return` semantics are confirmed unaffected.

`MUTSU_REAL_TEST=1 prove -e target/debug/mutsu t/throws-like-gather-sink.t`
(the ADR's acceptance gate) reaches 4/4 — the remaining 3 subtests (needing
the actual targeting, past the sink-forcing fix Slice 3 already landed) now
pass for the right reason. `t/emit-done-controlflow.t`, the other file the
origin ticket named, was already closed by an unrelated earlier fix; this
sweep re-confirmed it rather than needing new work.

Pins: `t/eval-context-live-target.t` (the ADR's own two-deep repro, the
named-light-dispatch twin, and a three-deep chain), and
`t/eval-context-slice5-residue.t` (`CALLERS::`, `$*THROWS-LIKE-CONTEXT`, and
`EVALFILE`). Full local `t/` suite, `cargo clippy -- -D warnings`, and
`cargo fmt` all clean; `roast/S04-statements/return.t` (test 15 specifically
— the recorded reason `enclosing_routine_exists()` exists at all) and the
full Slice 1-3 pin set stayed green throughout.

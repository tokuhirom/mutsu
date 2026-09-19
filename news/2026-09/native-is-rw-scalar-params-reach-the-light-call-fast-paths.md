# A scalar `is rw` positional parameter reaches the light-call fast paths, closing #8686 Phase 2

[#8686](https://github.com/tokuhirom/mutsu/issues/8686) Phase 2, against
[ADR-0109](../../docs/adr/0109-native-is-rw-scalar-parameter-reaches-the-light-call-fast-paths.md):
a plain `$` scalar positional parameter whose only trait is `rw` — native (`int`/`str`/`num`) or
untyped — now reaches `call_compiled_function_positional_light_at` instead of being excluded to the
general binder outright. This is the actual blocker behind [#8673](https://github.com/tokuhirom/mutsu/issues/8673)
(`JSON::Fast`'s `from-json` timing out on a 332KB document): every one of its scan helpers
(`nom-ws`, `parse-string`, `parse-thing`, ...) declares its cursor as `int $pos is rw`, so Phase 0
(native lowercase type recognition, [#8782](https://github.com/tokuhirom/mutsu/pull/8782)) alone
never touched this workload at all — the blanket trait exclusion forced every one of these helpers
onto the general binder regardless of its parameter types.

## What changed

Two gates, mirroring the ADR's four parts:

- **`vm_call_eligibility.rs`'s `is_positional_light_call_eligible`** narrows the trait exclusion:
  a positional `$`-sigil parameter's trait list may now be empty *or* exactly `["rw"]`, instead of
  only empty. `is_light_call_eligible` (the mixed named+positional light path) is intentionally
  **not** touched — `JSON::Fast`'s helpers are all-positional, and extending the mixed path's own
  bind loop the same way is a separate slice with its own binder plumbing to add.
- **A new runtime, per-call admission check, `positional_light_rw_args_admitted`**, re-derived on
  every dispatch (both the three cold eligibility call sites and the hot name-keyed
  `pos_light_call_cache` dispatch — the same "must re-check, not just re-check-once" shape
  `positional_light_full_arity_call` already established for a light-servable-only-at-full-arity
  routine, since the cache serves every call shape a call site uses for the same name). It admits
  only: a plain lexical read or plain-assignment-expression argument whose target the compiler
  resolved to a local of the *caller's* own frame (`Value::varref_slot()` — the tag every
  positional argument already carries via `WrapVarRef`, unconditionally, for every call, light or
  not), or an argument that is already a shared cell relayed from an outer `is rw` parameter. A
  native `int` parameter is admitted only when the argument's value already has the exact native
  shape (a plain `Int`, not a `Bool`/`BigInt` needing `wrap_native_int_for_binding`'s coercion) — a
  non-native, non-empty type constraint (`Int $x is rw`, `Str $x is rw`, ...) declines every call to
  that routine, permanently: it needs the general binder's type-check-then-box ordering, which this
  slice does not replicate. Declining costs only a speedup: the call falls through to the general
  binder exactly as it did before this ADR, with the same observable result.
- **Binding reuses `capture_var_cell` verbatim** — no new opcode, no new storage mechanism. A new
  pre-pass in `call_compiled_function_positional_light_at`, run *before* the callee's frame is
  pushed (`capture_var_cell` indexes the CALLER's own locals by the slot its `WrapVarRef` site
  resolved, so it has to run while `self.locals` still denotes the caller's frame, not after), turns
  each admitted `is rw` argument into a `ContainerRef` cell shared with the caller's own local slot
  and env entry. From there, nothing about reading or writing that parameter changes: a local slot
  holding a `ContainerRef` already stores through the cell and `GetLocal` already derefs one (the
  same mechanism closures, `Capture` literals, and `is rw`/`<->` loop parameters already use) — so
  there is no return-time writeback step to add. The parameter is also the one this bind loop does
  not mark readonly, mirroring the general binder's own "readonly unless `rw`/`copy`/`raw`" rule.
- Every `is rw` parameter's stack slot is skipped by the earlier type-check loop entirely once the
  pre-pass has promoted it: it is no longer the `VarRef`-wrapped value that loop's checks expect to
  unwrap, and admission already validated whatever the declared type requires.

## A subtlety the implementation had to get right: `self.locals` frame ordering

`call_compiled_function_positional_light_at` opens the callee's frame (`self.locals.push_frame`)
early, before either loop that walks `param_defs`, because "nothing between here and the parameter
bind reads `self.locals`" — true before this change. `capture_var_cell` is the one thing that does:
it resolves the caller's `name`/`slot_hint` against `self.locals[idx]` directly, with no frame
indirection of its own. Running it after `push_frame` would silently box a slot in the *callee's*
just-opened, still-`Nil`-filled frame instead of the caller's — so the alias-capture pre-pass runs
strictly before that push, reading the stack's still-`VarRef`-wrapped arguments and writing the
resulting `ContainerRef` back onto the stack in place, for the (unchanged) frame-push and bind loops
below to pick up as an ordinary already-boxed value.

## Measured

Release build, the ADR's own synthetic SPDX-shaped reproduction (`from-json` over 25–727 flat
records, seeded so the shape is reproducible):

| records | before (Phase 0 only) | after | speedup |
| --- | --- | --- | --- |
| 25 | 0.13s | 0.065s | 2.0x |
| 50 | 0.26s | 0.125s | 2.1x |
| 100 | 0.51s | 0.24s | 2.1x |
| 200 | 1.04s | 0.49s | 2.1x |
| 400 | 2.14s | 1.02s | 2.1x |
| 727 | 4.14s | 1.85s | 2.2x |

Scaling stays linear (doubling records still roughly doubles time), and `raku` on the same box parses
the 727-record document in 0.041s — mutsu is now ~45x slower than `raku` at that size, down from
~100x before this change. The remaining gap is Phase 3's territory (the ~10% raw `malloc`/`free`
share the original callgrind profile found), not this slice's.

Confirmed via `rust-gdb` (not just correct output, per the ADR's own acceptance bar — Phase 0's
history is the reason for that bar) that `parse-thing`, `parse-string`, and `parse-numeric` — real
helpers from the vendored `JSON::Fast` module, loaded and run end to end — now actually reach
`call_compiled_function_positional_light_at` with `has_rw_positional_param` set, not merely produce
the same answer via the general binder.

## Two regressions caught before landing — both by `make test`/`make roast`, neither by the new file's own first draft

Phase 0's own history ("two regressions caught only by `make test` and the CI battery gate")
repeated here, at smaller scale, exactly as CLAUDE.md's risk framing predicts for this kind of
change:

- **Marking a parameter writable is not the same as making it writable.** The bind loop's first
  draft simply *withheld* the `mark_readonly` call for an `is rw` parameter — correct-looking, and
  it passed every test this slice's own new file had at that point. `make test`'s full `t/` sweep
  caught it: `t/io/copy-param-shadows-caller-readonly.t`'s `sub bump($x is rw) { $x += 100 } sub
  via($x) { my $y = $x; bump($y); $y }` failed with "Cannot assign to a readonly variable", because
  the readonly set is keyed by bare NAME and shared across frames — `via`'s own (readonly) `$x`
  parameter had already marked the name "x" readonly, and merely skipping this frame's own mark left
  that outer mark in effect. The general binder gets this right by calling `unmark_readonly_sym`
  explicitly (`binding_signature.rs`, with a comment stating exactly this reasoning); the bind loop
  now does the same, relying on the same `ReadonlyFrameGuard` journal/restore machinery already
  active for this call to put the caller's own mark back on return.
- **An uninitialized variable is a container, not a value.** `capture_var_cell` (the plain form) is
  deliberately built to decline boxing a *bare type object* — that guard exists for List-aliasing
  (`Capture`/`\(...)` literals), where four distinct uninitialized `my` scalars must become four
  distinct containers, not accidentally share one. Reusing it verbatim for `is rw` binding inherited
  that decline: an uninitialized `my $var;` (holding `Any`) got handed back as the bare `Any` value
  with no cell at all, so a callee's write landed only in its own detached parameter slot and the
  caller's variable stayed `Any` forever. `make roast` caught it —
  `roast/integration/advent2011-day16.t`'s own `sub set_five($x is rw) { $x = 5 } my $var; set_five
  $var; is $var, 5` is exactly this shape. The fix is `capture_var_cell_boxing_type_objects` instead
  (the sibling form built for precisely "this bare type object is still a real container"), which
  this ADR's own point 3 had already named as an existing primitive without spelling out which of the
  two `capture_var_cell` entry points a parameter alias needed.

Both are now pinned in `t/routines/signature/positional-light-call-rw-param-alias.t` alongside the
file's other assertions, so a future regression on either shape fails locally, not three suites away.

## Two things this slice deliberately does not fix

- **`nom-ws`/`nom-comment`/`parse-true`/`parse-false` still take the general binder.** All four
  declare an explicit return type (`--> Nil`/`--> True`/`--> False`), and
  `is_positional_light_call_eligible`'s *separate*, pre-existing return-type gate
  (`cf.return_type.as_deref().is_none_or(Self::is_fast_type_name)`) has never recognized `Nil` (or a
  literal boolean spelling) as a fast-checkable return type, for any signature, `is rw` or not. This
  is an orthogonal gap in the return-type eligibility check, not the parameter-aliasing mechanism
  this ADR's Phase 2 is about; the other ten `is rw` helpers `JSON::Fast` declares (`parse-string`,
  `parse-numeric`, `parse-obj`, `parse-array`, `parse-thing`, and their `-immutable` siblings) have no
  return annotation and are unaffected.
- **A non-native boxed type constraint on an `is rw` parameter (`Int $x is rw`, ...) stays on the
  general binder unconditionally.** Scoped out by the ADR's own Decision: it needs the general
  binder's type-check-then-box ordering (check the *original* value's type, THEN promote it to a
  cell), which this slice's pre-pass — built to run once, before the frame push, and turn a `VarRef`
  straight into a cell — does not reproduce. `JSON::Fast` never declares one.

## An aside on real Rakudo's own behavior here

A native `int is rw` parameter called with a caller argument that is not *itself* declared `my int`
(a `my $n` boxed `Int` Scalar, a `Bool`, ...) does not raise a graceful type error in the Rakudo used
to measure this (2026.07) — it aborts with `Internal error: inconsistent bind result`. `JSON::Fast`
never hits this: its own `$pos` is always `my int $pos`, or itself an `int ... is rw` parameter of the
enclosing helper, at every call site. mutsu's general binder is (and, before this change, already
was) more permissive than that — it coerces a `Bool` argument through `wrap_native_int_for_binding`
and aliases the result — and this slice preserves that existing general-binder behavior verbatim for
any call its own admission check declines; it does not attempt to newly match Rakudo's own internal
error for a case Rakudo cannot itself run to completion.

## Pinned

`t/routines/signature/positional-light-call-rw-param-alias.t` (15 assertions): both `JSON::Fast` call
shapes (a plain lexical argument, and the assignment-expression shape
`parse-string($text, $pos = $pos + 1)`), that the alias is live across repeated calls (not a one-shot
copy), two `is rw` parameters aliased independently in one call, a sibling non-`rw` parameter staying
readonly, the cached dispatch path correctly re-declining a later, differently-shaped call at the same
routine name (a literal, after two lexical-argument calls already populated the cache) without
disturbing the routine's own correctness on the next admitted call, and every DECLINE shape (a
literal, an array element, a method-call result, a Bool into a native `int` parameter) reproducing the
general binder's exact prior behavior.

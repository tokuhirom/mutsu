# ADR-0109: A native `is rw` scalar parameter reaches the light-call fast paths by reusing the existing `WrapVarRef`/`ContainerRef` transport, restricted to plain-lexical arguments

- Status: Proposed
- Date: 2026-09-19
- Related: [ADR-0067](0067-a-routine-hands-back-the-container-it-was-given.md)
  (the general binder's container-transport machinery for `is rw`/`is raw`
  arguments and invocants — the mechanism this ADR reuses rather than
  reinvents), [ADR-0032](0032-wrapvarref-container-capture-across-closure-boundaries.md)
  (`WrapVarRef` capture semantics)
- Addresses: [#8686](https://github.com/tokuhirom/mutsu/issues/8686) Phase 2
  (the issue's own "needs a `Proposed` ADR before any implementation attempt"
  gate). Phase 0 (native lowercase type recognition, #8782) and Phase 1's
  second bullet (#8693) are already shipped; Phase 1's first bullet is
  tracked separately as [#8690](https://github.com/tokuhirom/mutsu/issues/8690).
  This ADR does not touch either.

## Context

#8686 measured that `JSON::Fast`'s own `from-json` costs ~2.46 billion
retired instructions to parse 100 flat records, scaling linearly with input
size (so this is a large per-call constant, not a quadratic blowup), and
that `raku` parses the real 332KB/727-record document (#8673) in well under
a second where mutsu takes ~4.1s. Root-caused to two independent gaps in
`vm_call_eligibility.rs`'s fast-path admission gate
(`is_light_call_eligible` / `is_positional_light_call_eligible`):
unrecognized native lowercase type spellings (`int`/`str`/`num`) — closed by
Phase 0 (#8782) — and the blanket exclusion of any parameter carrying a
trait (`pd.traits.is_empty()`), which excludes `is rw` and therefore every
helper `JSON::Fast` actually declares (`nom-ws`, `parse-string`,
`parse-thing`, ...: all take their scan position as `int $pos is rw`).
Phase 0 alone does not move `JSON::Fast`'s own workload at all, because
every one of its hot helpers is excluded by the trait check regardless of
its parameter types.

This ADR is Phase 2: admit a native `is rw` scalar parameter to the light
paths (`vm_call_light.rs`, `vm_call_light_typed.rs`), for the shape
`JSON::Fast` and the wider nqp::-style ecosystem code actually use. The
light paths bind every parameter today by copying a value into a local
slot, with **no aliasing back to the caller's variable at all** — that is
exactly why the trait check excludes `is rw` in the first place, and is the
gap this ADR closes.

## What already exists — measured, not assumed

Before designing new machinery, the actual call-argument compile path and
the general binder's existing `is rw` handling were read end to end, because
#8686 itself warns that this exact kind of change "looks like a one-line
equivalence table addition but is only correct under an incomplete model" —
Phase 0's own history (two regressions caught by `make test` and the CI
battery gate after a first attempt looked done) is the concrete precedent
for taking that warning seriously here.

1. **Every plain-lexical or plain-assignment positional argument is already
   tagged at the call site, for every call, regardless of the callee or
   which binder eventually processes it.** `compile_call_arg_with_escape`
   (`src/compiler/helpers_call_args.rs:612-721`) matches `Expr::Var`,
   `Expr::AssignExpr`, `Expr::CompoundAssign`'s expanded assign, and an
   inline `Expr::DoStmt(VarDecl)` — exactly the two shapes `JSON::Fast`
   itself uses (`nom-ws($text, $pos)` and `parse-string($text, $pos = $pos +
   1)`) — and emits `emit_wrap_var_ref_arg_tag` unconditionally "for EVERY
   arg shape ... purely to tag its shape for LATER is-rw dispatch matching"
   (the function's own doc comment). This is not new work to add; it is
   already running on every call today, light-path calls included — the
   light paths simply never look at the tag.
2. **The tag is cheap and does not eagerly allocate a container.**
   `exec_wrap_var_ref_op` (`src/vm/vm_misc_assign.rs:767`) pops the argument
   value and pushes `Value::varref_slotted(sym, value, None, Some(slot))` —
   a `ValueView::VarRef { value, name, slot }` wrapper carrying the
   compile-time-resolved local slot verbatim, including the `u32::MAX`
   "not a local of this frame" sentinel. Its own doc comment records that
   this is "the single hottest opcode in a call-heavy program (99.7% of
   `bench-tak`'s interpreted ops)" and must not allocate beyond the one
   payload — i.e. this transport was already built to be light-path-grade
   cheap.
3. **Promoting a tagged plain lexical into a real, shared `ContainerRef`
   cell is one existing, reusable primitive.**
   `Interpreter::capture_var_cell` / `capture_var_cell_inner`
   (`src/vm/vm_data_ops.rs:335-420+`) already handles: reusing an existing
   cell when the slot already holds one, following a `:=`-alias-chain root
   (`resolve_alias_root`) so every alias of a binding shares one cell, and
   minting a fresh cell over the named local's current value otherwise. It
   is already the mechanism `MakeArray`/`Capture` literal elements and
   `return-rw` use to alias a caller's scalar.
4. **A local slot holding a `ContainerRef` already reads and writes through
   the cell with no extra machinery.** This is stated directly in
   ADR-0067's slice 3b writeup: "A local slot holding a `ContainerRef`
   already stores through the cell (`vm_var_assign_set_local.rs`,
   `vm_var_assign_local.rs`) and `GetLocal` already derefs one." Nothing
   about storing or reading the *parameter's own slot* needs to change once
   it holds the right value — this is exactly how a closure's captured
   outer scalar and an `is rw`/`<->` loop parameter already work.
5. **The general binder's actual `is rw`/`is raw` handling is
   substantially more than "call `capture_var_cell`."**
   `src/runtime/types/binding_signature.rs` (~line 2620 onward) also
   maintains a *second*, alternative mechanism — `pending_rw_writeback_slots`
   plus `resolve_sigilless_alias_source_name` — a bind-by-value,
   writeback-on-return protocol used when a true shared cell cannot or
   should not be minted (a relayed sigilless parameter, an `is raw`
   parameter whose argument is a type object or a readonly binding, both of
   which the code calls out by name as `implicit_raw_veto` /
   `raw_readonly_source`). This is real, deliberately-built correctness
   machinery (ADR-0067's slices exist because of exactly these edge cases),
   not incidental complexity, and #8686's own "Gap 1 turned out to be
   non-trivial too" section is a direct precedent: even the "obviously
   safe" native-type recognition needed two more pieces of general-binder
   behavior ported before it was correct. The same pattern applies here, at
   larger scale, if this ADR tried to make the light paths fully equivalent
   to the general binder's `is rw`/`is raw` handling in one slice.

## Decision

**Scope Phase 2 to exactly the shape #8673/`JSON::Fast` needs — a plain `$`
native or untyped scalar parameter whose only trait is `rw`, bound from a
plain lexical or plain-assignment argument — by reusing the existing
`WrapVarRef` tag and `capture_var_cell` primitive verbatim. Everything the
general binder additionally does for `is raw`, for a non-lexical argument
source, or for a relayed/aliased binding stays out of scope: the light path
must safely DECLINE (fall through to the general binder) rather than
attempt to replicate it.**

Concretely, four parts:

### 1. Eligibility gate: narrow the trait exclusion, do not remove it

In both `is_light_call_eligible` and `is_positional_light_call_eligible`
(`src/vm/vm_call_eligibility.rs`), replace the blanket `pd.traits.is_empty()`
clause, for a positional `$`-sigil parameter, with: the parameter's trait
list is either empty, or exactly `["rw"]`. Every other existing clause in
the gate is unchanged — in particular the parameter must still pass
`is_fast_type_name` (so Phase 0's `NativeInt`/`NativeStr`/`NativeNum`, or a
boxed `Int`/`Str`/`Num`/`Bool`/`Rat`/`Any` type constraint), must not be
named/slurpy/sigilless/invocant/attributive, and the routine itself must
still be `!cf.is_rw && !cf.is_raw` (a routine-level `is rw`, i.e. a
container-*returning* routine, is untouched by this ADR — that is a
different mechanism, ADR-0059/ADR-0067's territory, not a parameter-level
`is rw` argument).

`is raw` and a sigilless (`\x`) parameter are explicitly NOT part of this
slice (see Rejected alternatives).

### 2. Runtime, per-call admission of the actual argument shape

A signature-only gate is not enough: whether a specific *call* actually
supplies an alias-capable argument is a property of that call site, not of
the callee's signature. This repo already has a precedent for a per-call
(not purely per-signature) light-path admission check —
`Interpreter::positional_light_full_arity_call` inspects the actual `args`
slice (its `Pair` scan) before admitting a call with omitted optional
positionals. This ADR adds the same kind of check for an `is rw`-parameter
position: admit only when the argument value at that position is one of

- a `ValueView::VarRef { slot, .. }` with `slot != u32::MAX` (a plain lexical
  read or a plain-assignment-expression argument whose target is a local of
  the CALLER's own frame — both `JSON::Fast` shapes), or
- an argument that is already `ValueView::ContainerRef(_)` /
  `ValueView::HashEntryRef { .. }` (already boxed by an earlier step, e.g. a
  relayed rw parameter).

Decline — meaning: this one call takes the general binder, exactly as
today, with no new error path and no behavior change — for every other
shape: an accessor-method-call argument, a subscript element, a literal, a
`VarRef` with no resolvable local slot (a package/global-qualified name, an
outer/captured lexical not in this frame), or a sigilless bareword. Declining
only ever costs a speedup, never correctness, which is the same safety
direction #8686 itself insists on ("admitting a scalar `is rw` parameter
without \[a writeback mechanism\] would silently break every mutation" —
the light path must never guess).

### 3. Binding: reuse `capture_var_cell`, no new storage mechanism

When a parameter passes both gates, call `Interpreter::capture_var_cell`
(unchanged, not reimplemented) with the `VarRef`'s name/slot to obtain a
`ContainerRef`, and store that `ContainerRef` in the parameter's own local
slot exactly the way any other light-bound value is stored today. No new
opcode and no new read/write path: point 4 above is why the existing
`GetLocal`/`SetLocal` family already does the right thing once the slot
holds a `ContainerRef`.

### 4. Native coercion does not apply to an aliased `is rw` argument

Phase 0's bind-time coercion (`wrap_native_int_for_binding`, e.g. `Bool` ->
`Int`) copies a value into a fresh native slot; it must **not** run on an
`is rw` argument, because coercion would fork the representation away from
the caller's own storage and silently break write-visibility (the callee
would mutate a copy, not the alias). This needs a measurement against
`raku`/rakudo before implementation — e.g. what `sub f(int $x is rw) {}; my
$s = "5"; f($s)` actually does (refuse to bind, or bind and coerce
bidirectionally is implausible for a native representation) — and is listed
as an open question below rather than assumed here.

Declared-type env metadata (Phase 0's `~~ int`-style introspection gate,
`CompiledCode::mentions_native_scalar_type_name`) is orthogonal to aliasing
and applies unchanged.

## Rejected alternatives

- **Port the general binder's full `is rw`/`is raw` machinery
  (`pending_rw_writeback_slots`, `resolve_sigilless_alias_source_name`,
  `implicit_raw_veto`/`raw_readonly_source`) into the light paths
  unconditionally.** This would be correct but defeats the point: the light
  path exists specifically to skip exactly this bookkeeping for the common
  case. Paying it on every `is rw` call would tax the very call shape this
  ADR exists to speed up. Rejected in favor of the narrower gate in
  Decision §2, which declines to the general binder — and therefore to this
  exact machinery — whenever the call does not match the simple shape.
- **A new, light-path-specific aliasing mechanism (a new opcode, a new
  value representation).** Unnecessary: `WrapVarRef` / `ContainerRef` /
  `capture_var_cell` already do this job, are already exercised by
  closures, `Capture` literals, and `is rw`/`<->` loop parameters, and are
  already allocation-frugal by design (point 2 above). Building a parallel
  mechanism would duplicate a proven one for no benefit, contrary to the
  reuse-over-reinvent conclusion each of ADR-0067's slices (3a, 3b, the E6
  producer) independently reached after measuring the alternative.
- **Admit `is raw` / sigilless (`\x`) parameters in the same slice.** `is
  raw`'s implicit-veto rules — declining a type-object source or a readonly
  binding source — are judgment calls the general binder makes today
  (`implicit_raw_veto`, `raw_readonly_source`) that the runtime admission
  check in §2 does not reproduce. `JSON::Fast`'s own helpers only need `is
  rw`; a `\x`/`is raw` follow-up can reuse the same transport once this
  slice's boundaries are measured and stable.
- **Admit non-lexical argument shapes (an accessor read, a subscript
  element) at the same time.** These are exactly the cases ADR-0067 needed
  several corrected iterations to get right (the E6 producer, the 3a/3b
  split, `MarkRwArgRefContext`/`MarkRwArgRefContextCallee`'s
  callee-resolution gates). None of it is needed for `JSON::Fast`'s
  plain-lexical helpers, and admitting it now would import that same
  correction cost into a slice that does not need it.

## Consequences

- `JSON::Fast`'s own helpers (`nom-ws`, `parse-string`, `parse-thing`, ...)
  become eligible for the positional-light path, which is the actual
  #8673 blocker Phase 0 alone could not touch — *provided* every call site
  passes a plain-lexical or plain-assignment argument at the `is rw`
  position, which is the shape the vendored module uses throughout (per
  #8686's own citation of both call shapes).
- A call site that passes something else at that position (rare; should be
  confirmed by grepping the ecosystem corpus before implementation) keeps
  using the general binder with no behavior change — it simply does not
  gain the speedup.
- `pos_light_call_cache` is keyed by routine NAME and already has to serve
  every arity/argument-shape a call site uses for the same name (see
  `light_full_arity_only`'s own doc comment on this). Whether the §2
  runtime shape check belongs in the one-time cache-population gate or must
  be re-derived on every cached dispatch is an implementation-time question
  (see Open questions) — getting it wrong in the "only checked once" direction
  would let a later call with a non-lexical argument silently reuse a
  container-binding decision made for an earlier, lexical-argument call at
  the same name.

## Acceptance the implementation PR must show

- `sub f(int $x is rw) { $x += 1 }; my $n = 41; f($n); say $n;` -> `42`,
  and confirmed (via a counter or `--dump-bytecode`/trace, not merely
  correct output) that the call actually took the light path — Phase 0's
  own history shows a change can look like it works while quietly missing
  the fast path entirely.
- Both `JSON::Fast` call shapes — `nom-ws($text, $pos)` and
  `parse-string($text, $pos = $pos + 1)` — verified to take the light path
  and to produce output byte-identical to `raku`.
- Regression coverage for every DECLINE case in Decision §2 (accessor
  argument, subscript argument, literal argument, sigilless bareword,
  outer/captured lexical, package-qualified name): each must produce output
  identical to before this change. Phase 0's own experience — two silent
  regressions caught only by `make test` and the CI battery gate — is the
  reason this is listed as a hard requirement, not a nice-to-have.
- A re-run of #8686's own callgrind/wall-clock `JSON::Fast` reproduction
  (25/50/100/200/400/727 synthetic records), with the resulting per-record
  instruction/wall-clock numbers reported against both the pre-Phase-2
  baseline and `raku` on the same box.
- `make test` and `make roast` both green, per this repo's own
  pre-publication gate (CLAUDE.md "Run both full suites yourself before
  publishing a PR").

## Open questions for the implementation PR (deliberately not resolved here)

1. **Bind-time behavior for an incompatible source value.** What must
   happen for `sub f(int $x is rw) {}; my $s = "5"; f($s)` — measure against
   `raku` first, per the reasoning in Decision §4, rather than assuming a
   refusal shape.
2. **Where the runtime shape check of Decision §2 lives relative to
   `pos_light_call_cache`'s per-name caching** — once at cache-fill time, or
   re-checked on every cached dispatch. Resolve by reading how that cache
   is populated and revalidated today (`light_full_arity_only`'s own
   mechanism is the closest existing precedent) before choosing, rather
   than guessing which is cheaper.
3. **Whether `capture_var_cell`'s alias-chain-root resolution
   (`resolve_alias_root`) needs to run for this slice's restricted argument
   shapes at all**, or whether a plain lexical/assignment argument (as
   opposed to a `:=`-bound one) can skip that lookup entirely — worth
   measuring, since #8686's whole premise is that this call shape is
   extremely hot and every unnecessary lookup shows up in the profile.

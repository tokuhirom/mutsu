# ADR-0019 E4b: decomposing `should_bypass_native_fastpath` for the resolver cutover

**Status update (2026-08-11):** steps 1-3 below are answered — see the ADR's
E4b "Progress 2026-08-11" notes (step-1/2/3 entries). Step 1:
`resolve_user_method_or_accessor` shadow-verified NOT to subsume category 2
(`is_native_method`) — confirmed via a `t/`-wide sweep, 99.95% of shadow
mismatches were `Supply.tap`-shaped (a pure native method with no matching
accessor). Step 2: category 1 does **not** reduce to "the row table has no
entry" the way this doc originally hoped, because the adopted gate
renegotiation makes E4b's resolver fall back to the cascade on any row miss
— row absence no longer implies "the resolver skips it." Step 3: category 2
now has its own `ResolvedCandidate::NativeCallBinding` kind in
`resolution_sequence.rs`, shadow-verified against the same sweep methodology
— mismatches fell from 4172/20635 (20.2%) to 34/20634 (0.16%), and the
residual 34 are the pre-existing category-3-only shape already noted in step
1, not category 2. What's left before the actual authoritative switch:
implement category 1's guard list (step 2's per-case disposition) as explicit
resolver guards, and add design decision 4's E2-row-catalog `Native`
candidate variant (a fourth kind, distinct from `NativeCallBinding`) so the
switch has every candidate kind it needs.

E4b ("authoritative switch at the cached-resolve boundaries, native rows
included, `should_bypass_native_fastpath` deleted") is the next unstarted box
in the Phase E sequence, following E4a (landed 2026-08-10: the shadow-mode
user-candidate sequence builder, `src/runtime/resolution_sequence.rs`). This
note captures a concrete scoping investigation done ahead of implementation,
so a future session can start coding instead of re-deriving this analysis.

## The call site is smaller than the ADR bullet suggests

`should_bypass_native_fastpath` (`src/runtime/methods_native_bypass.rs:116`)
has exactly **one caller**, in `Interpreter::call_method_with_values`
(`src/runtime/methods_call_dispatch.rs:2784`). That function is the primary
interpreter *slow-path* method-dispatch entry point (see CLAUDE.md's
"Method dispatch (two-tier)" section). Its boolean result gates whether the
call proceeds through the pure `native_method_{0,1,2}arg` cascade or skips
straight to the interpreter's own further resolution logic below (which
falls through to `resolve_user_method_or_accessor`-style lookups, class
introspection, etc.).

E4b's "authoritative switch" is therefore about replacing *this one decision
point*, not a sweep across many call sites — E5/E6/E7 (separately scoped,
unstarted) are what cover the VM opcode / mutation-aware / metaobject entry
points. This makes E4b considerably more tractable than its ADR bullet
implies at a glance.

## The ~110-line function decomposes into three unrelated categories

Reading the full body (`methods_native_bypass.rs:116-225`), the boolean OR
chain is not one policy — it is three different questions bundled together:

1. **"Is native dispatch unsafe for this receiver shape here, independent of
   any candidate?"** — the Match/Supply/Proc::Async/IO::Handle/Stash special
   cases (lines 130-213), plus the Real/Numeric bridge arm (line 184) and the
   exception-message-computed-lazily gate (line 176). These are about the
   *correctness of the native fast path itself* for that receiver, not about
   candidate priority. They likely need to remain as explicit escape hatches
   even after the cutover — or (better, and worth investigating first) get
   modeled as those owners simply having **no native row** at all for the
   affected methods (`NativeRowFlags::SPECIAL`, already how E2a/E2b classify
   "not servable by the pure arity cascades"), so the resolver naturally
   skips native without a bespoke gate. Whether every one of these cases can
   be re-expressed that way is unverified — needs a case-by-case check
   against the row table.

2. **"Does this class have an explicit NativeCall C-function binding for
   this name?"** — `self.is_native_method(&class_name.resolve(), method)`
   (line 180 and its `Package`-receiver twin). This is `ClassDef::native_methods`
   (`is native(&sym)` trait bindings), a **third kind of candidate** distinct
   from both `ResolvedCandidate::User` (E4a) and the E2 native-row table —
   neither exists in `resolution_sequence.rs` yet. `is_native_method` is its
   own small, self-contained lookup (`class_introspection.rs:63`); wiring it
   into the sequence (a `ResolvedCandidate::NativeCallBinding` variant, or
   folding it into `resolve_user_method_or_accessor`, see below) is probably
   the smallest of the three categories.

3. **"Does a user method/accessor/class-level-attr win at some MRO level?"**
   — `has_user_method`/`has_public_accessor`/`has_class_level_attr`, each
   called *separately* per receiver kind (Instance/Package) at lines
   214-224. This is exactly the question **`resolve_user_method_or_accessor`**
   (`class_introspection.rs:280`) already answers, in one MRO walk, with the
   correct per-level priority (local method > role method > public accessor,
   class entities over role entities) — and folds in the NativeCall-binding
   check from category 2 as well (`has_native` at line 302). It is already
   production code, consumed at 5 call sites (`methods_instance_ops.rs`,
   `methods_mut_method_lvalue.rs`, `vm_call_method_compiled_interpret.rs`,
   `vm_call_method_ops.rs` x2) — not shadow-only, not new, not unproven.

## What this suggests for an implementation plan

The smallest safe first slice is probably **not** "port everything to
`resolve_sequence`" but:

1. Verify (case by case, probing real values the way every E2b slice did)
   whether `resolve_user_method_or_accessor`'s answer at the call site's
   *own* class level already subsumes categories 2 and 3 above — i.e.
   whether `should_bypass_native_fastpath`'s lines 214-224 can be replaced
   outright by one `resolve_user_method_or_accessor` call, in shadow mode
   first (a `MUTSU_VM_STATS` counter comparing the two answers over a full
   `t/` + whitelisted-roast sweep, mirroring E1a/E4a's own methodology),
   before touching anything live.
2. Separately audit category 1's ~8 special cases against the row table: for
   each, does the affected owner/method combination already have no
   `native_method_row` entry (or could reasonably be given the `SPECIAL`
   flag) such that a resolver consulting the row table would naturally never
   route there? Any case that does NOT reduce this way needs a decision
   documented in the ADR (kept as a hardcoded gate is fine and not a
   compromise — see decision 3's admission-gate classification table — but
   it should be an intentional, recorded choice per case, not a leftover).
3. Only once both are shadow-verified at zero mismatches does the actual
   authoritative switch (deleting `should_bypass_native_fastpath`, routing
   `call_method_with_values` through the resolver's decision) become the
   safe, final, small diff E4b's bullet describes.

## Relationship to the E2b counter-to-zero gate

See the ADR's "Gate-renegotiation proposal" note (added alongside E2b's
twelfth slice, 2026-08-10): the design doc requires `native_call_unmodeled`
to be exactly zero before E4b lands. After twelve E2b slices the counter is
down ~99% (~37904 to ~400) with no dominant cluster left in the remainder —
diminishing returns on chasing the last one-offs. The proposed resolution
(not yet adopted) is for E4b's resolver to fall back to the existing cascade
on any row miss while continuing to increment the counter, turning "zero"
from a hard precondition into an ongoing monitored signal. That decision is
orthogonal to the decomposition work above and should be made before or
alongside step 3, not blocking steps 1-2 (which are shadow-only and touch no
production dispatch decision regardless of the counter's value).

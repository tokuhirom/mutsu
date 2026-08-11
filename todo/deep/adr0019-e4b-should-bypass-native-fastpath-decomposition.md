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
1, not category 2. **Step 4 (scoped, not yet implemented — see "Design
decision 4's `Native` candidate needs a bigger signature change than it
looks" below):** the E2-row-catalog candidate is not a same-shape follow-on
to step 3. It needs (a) `resolve_sequence`'s signature to grow the design
doc's `shape: CallShape` and `definedness` parameters — `CallShape` does not
exist as a type anywhere in the codebase yet, it is purely a design-doc
concept from `todo/deep/adr0019-e2-e4-resolver-core.md`'s E3 cache-key
sketch — because row coverage is arity/definedness-dependent in a way
`NativeCallBinding`'s plain boolean membership is not, and (b) several
`native_method_row.rs` items un-gated from `#[cfg(test)]`
(`NativeRowFlags::TYPE_OBJECT_OK`/`MUTATES_RECEIVER`/`contains`, and a new
unambiguous row-existence predicate — see below). Left unimplemented this
session by deliberate scope decision, not an oversight. Step 5: confirmed and
dropped the first of step 2's "likely reduces, not exhaustively proven"
category-1 guards — `Supplier`/`Supplier::Preserving.Supply` — live, ahead of
the resolver switch, since the finding ("the cascade's own `"Supply"` arm
already self-guards, `methods_0arg/coercion.rs:655-661`") makes the outer
`should_bypass_native_fastpath` gate provably redundant today, independent of
any resolver work. Verified with `cargo test --lib`, `prove -j4 t/`, and the
full `S17-supply` roast subset (99 files), all green with the guard removed.
The other two "likely reduces" groups (`Proc::Async`'s method family,
`Stash`'s `AT-KEY`/`keys`/`values`) and the "mixed" `IO::Handle`
`encoding`/`opened`/`DESTROY` group are still open — same methodology
applies: read the cascade arm(s) for the exact method name(s), confirm they
either don't exist for an arbitrary `Instance` or already self-guard for the
receiver in question, remove the guard, then verify empirically rather than
trusting the read alone (`cargo test --lib` + `prove t/` + the relevant
whitelisted roast files). What remains before the actual authoritative
switch: finish category 1's guard list (the two still-open groups above),
and design decision 4's `Native` candidate (step 4).

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
production dispatch decision regardless of the counter's value). (Adopted —
see the ADR's "Gate-renegotiation proposal" note.)

## Design decision 4's `Native` candidate needs a bigger signature change than it looks (step 4 scoping, 2026-08-11)

Step 3 added `ResolvedCandidate::NativeCallBinding` (category 2,
`is_native_method`) cheaply because it is a plain per-`(owner, name)` boolean
fact — the same shape `resolve_sequence`'s existing `User` candidates already
have. Design decision 4's `Native` variant
(`todo/deep/adr0019-e2-e4-resolver-core.md`, the `ResolvedCandidate` sketch)
looked like the same kind of addition at a glance. It is not, for two
independent reasons found while scoping the follow-on slice:

1. **Row coverage is call-shape-dependent, not a flat boolean.** A
   `NativeMethodRow` carries a `NativeArityMask` (which of `native_method_0arg`/
   `_1arg`/`_2arg`/none actually serves the name) and `NativeRowFlags`
   (`TYPE_OBJECT_OK`: servable on an undefined type object; `MUTATES_RECEIVER`:
   really a Tier-A `&mut self` path, not the pure arity cascade; `SPECIAL`:
   intercepted ahead of the cascade or genuinely unmodeled). Whether a `Native`
   candidate should even be *offered* at a given call site depends on the
   call's arity and the receiver's definedness — exactly the two inputs design
   decision 4's own sketch signature already anticipates:
   `resolve_sequence(chain: &[TypeId], name: Symbol, shape: CallShape,
   definedness) -> Option<ResolvedSequence>` (`adr0019-e2-e4-resolver-core.md`
   line 168). Today's `resolve_sequence(chain, name)` (E4a's original
   two-argument shape, unchanged by step 3) has neither parameter — `User` and
   `NativeCallBinding` candidates don't need them (a user method's own
   signature is checked per-call by the existing ranker, and a NativeCall
   binding is a pure name-presence fact independent of arity). Adding `Native`
   without `shape`/`definedness` would force either (a) always including a row
   candidate regardless of whether the actual call could ever use it — junk
   candidates the ranker has to filter out with logic that duplicates the
   arity-mask/flag checks the row already encodes — or (b) reaching back into
   caller-supplied arity/definedness through some other side channel, which
   defeats the point of a self-contained sequence builder.
2. **`CallShape` does not exist as a type anywhere in the codebase.**
   `git grep CallShape` outside this doc and `adr0019-e2-e4-resolver-core.md`
   finds nothing — it is a design-doc sketch (`{ arity_bucket: 0|1|2|3+,
   has_named: bool }`, `adr0019-e2-e4-resolver-core.md` line 193) for E3's
   future cache key, never implemented. Threading it through
   `resolve_sequence` now means: defining the type, deciding whether E4b needs
   the full E3 cache-key shape or a smaller local subset, and updating every
   existing caller of `resolve_sequence`/`shadow_check_resolver`
   (`resolve_method_cached`'s two boundaries, step 3's
   `shadow_check_bypass_user_method_categories`) to pass it — a signature
   change to code that already landed and is shadow-verified, not a pure
   addition.
3. **The row-existence check itself is not production-safe yet.** The only
   production (`#[cfg(test)]`-free) entry point,
   `crate::builtins::native_method_row::native_method_row(owner, name) ->
   (NativeArityMask, NativeRowFlags)`, returns the *same* conservative
   `(N, SPECIAL)` default both when no row exists for the pair AND when a row
   genuinely exists and is deliberately classified `N`/`SPECIAL` (e.g. a
   name recognized at no arity at all, or a Tier-A mutator) — the two cases
   are indistinguishable from its return value alone. `NativeMethodRow` (the
   struct design decision 4's candidate wants to hold a `&'static` reference
   to) and `NativeRowFlags::{TYPE_OBJECT_OK, MUTATES_RECEIVER, contains}` are
   all `#[cfg(test)]`-gated — deliberately, per the module doc, since E2a's
   only reader today is its own inverse probe. A `Native` candidate needs a
   new, unambiguous, production-visible predicate (e.g.
   `native_method_row_exists(owner, name) -> bool` backed directly by
   `classification_table().contains_key(..)`, not by the existing
   conservative-default `native_method_row()`) before it can safely decide
   "does this level even have a catalog entry" — reusing today's function
   would misreport every genuine `(N, SPECIAL)`-classified row as absent, or
   worse, silently treat "absent" and "recognized-but-unservable" the same
   way a real dispatch decision cannot afford to.

None of this blocks step 3's `NativeCallBinding` (already landed, PR #6213)
or category 1's guard-list implementation (still open, independent of this
finding). It does mean `Native` is genuinely the last and most involved of
the four candidate kinds design decision 4 lists — plan a dedicated slice for
it (signature change to `resolve_sequence` + its callers, a new
production-safe row-existence predicate, then the usual shadow-verify sweep)
rather than expecting it to be a small mechanical follow-on to step 3.

**Update (step 9, 2026-08-11, landed):** the `Native` candidate is in
(`ResolvedCandidate::Native { owner }`, `resolution_sequence.rs`), smaller
than this note anticipated. `NativeCallShape { arity, definite }` is the
E4b-local subset threaded through `resolve_sequence`'s signature (point 1
above) rather than the full E3 `CallShape`. Point 3's "new row-existence
predicate distinguishing absent from genuinely-SPECIAL" turned out
unnecessary: `native_row_servable` (`native_method_row.rs`) only needs "is
this call's arity servable, non-`SPECIAL`, non-`MUTATES_RECEIVER`, and
`TYPE_OBJECT_OK` if indefinite" — both "no row" and "row classified special"
correctly answer "not servable" for that question, so only
`TYPE_OBJECT_OK`/`MUTATES_RECEIVER`/`NativeRowFlags::contains` needed
un-gating from `#[cfg(test)]`, not a new predicate or the `NativeMethodRow`
struct. Shadow-verified without a t/-wide sweep: a new
`shadow_check_native_row_candidate` compares the candidate's presence
against `native_result.is_some()` — the real cascade result
`call_method_with_values` already computes, not a second invocation, so no
double-invocation side-effect risk. See the ADR's step-9 progress note for
the full verification record. Still open: actually consuming `User`/
`NativeCallBinding`/`Native` together to replace `should_bypass_native_fastpath`
at its one call site — that is the authoritative switch itself, not attempted
here.

**Update (step 10, 2026-08-11, scoping finding — read before attempting the
authoritative switch):** `NativeCallBinding` does NOT generalize across
receiver kind the way `Native` already does. A widened shadow sweep (see the
ADR's step-10 note) found `resolve_sequence`'s presence-only
`NativeCallBinding` walk disagreeing with the real bypass decision 143 times
out of 20634 checks, always for a `Package` (type-object) receiver:
`ClassDef::native_methods` conflates instance-method names with class-level
factory-method names under one flag (`Supply.interval`/`Compiler.id`, answered
by a hardcoded special case in `methods_instance_ops.rs`, never reaching
`should_bypass_native_fastpath` at all), so `is_native_method`'s true answer
is irrelevant at a `Package` receiver — which is exactly why the real
category-2 term only ever checks it for `is_instance`. Whoever writes the
authoritative switch must gate `NativeCallBinding` (like `Native` already
gates on `definite`/`TYPE_OBJECT_OK`) by `is_instance`/`NativeCallShape::definite`
at the point it drives a real decision, not trust bare candidate presence.

**Update (step 12, 2026-08-11, landed):** the Instance branch's category-3
cutover from step 1's finding is live: `has_user_method(..) ||
has_public_accessor(..)` collapsed into one `resolve_user_method_or_accessor(..).is_some()`
call, with the function restructured into a `match target.view()` (one arm
per receiver kind) instead of the old flat OR-chain. Also decided,
explicitly, NOT to route category 2 (`is_native_method`) through
`resolve_sequence`'s `NativeCallBinding` candidate at this call site: both
compute the exact same MRO walk, so swapping in the resolver here would only
add the cost of building a full sequence (plus unused `User`/`Native`
candidates) for zero correctness gain — `NativeCallBinding` is for a future
multi-candidate consumer, not a drop-in replacement for a single boolean
fact already answered by a direct, cheaper call. See the ADR's step-12
progress note for the full verification record (`cargo test --lib`, `prove
t/`, a roast smoke subset). Still open: whether `Native` (the row-catalog
candidate) is ever worth consuming to replace the `native_method_{0,1,2}arg`
dispatch decision itself, given the same tradeoff likely applies there too —
check that before assuming there is more plumbing work before E4b is done.

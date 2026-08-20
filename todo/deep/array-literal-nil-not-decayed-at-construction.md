# Array-literal `[Nil]` keeps raw `Nil` instead of decaying to `Any` at construction — root cause of the RSV `eqv`/`is-deeply` gap, plus a second typed-array divergence

> **Status (2026-08-20): still open; the design is now decided in
> [ADR-0049](../../docs/adr/0049-nil-decays-to-the-container-default-at-the-element-store.md).**
> Read the ADR first — it is the authoritative account, and it corrects three things below. A
> re-verification against `main` (227e38e4f) confirmed every claim in this file still reproduces, and
> found the problem is both wider and structurally different from what is written here:
>
> - **The decay target is the *owning container's* default, not literally `Any`.** `nil_elems_to_any`'s
>   hardcoded `Any` is why the fixup has to gate itself off for typed arrays. Measured:
>   `my @d is default(42) = [Nil]` is `(Any)` in raku but `42` in mutsu, because mutsu applies the
>   *outer* container's default to a `Nil` that should already have decayed inside the `[Nil]` literal.
>   `Interpreter::typed_container_default` (`src/vm/vm_var_ops.rs:377`) is the ladder that already
>   exists and should be the single decay target.
> - **The real architectural weight is a sentinel collision this file does not mention.** `Value::NIL`
>   is simultaneously the stored value `Nil`, `resolve_hash_entry`'s "absent key", and
>   `ArrayData::hole_at`'s "deleted slot / gap" — while `Package("Any")` is the *intended* gap marker
>   everywhere else and `ArrayData::initialized` is already a precise hole discriminator. That is why
>   `[Nil,1][0]:exists` is `False` (raku: `True`) and why `[Nil].List.raku` is `(Nil,)` (raku: `(Any,)`)
>   — the same `.List` code is right for a real hole and wrong for a real element because mutsu cannot
>   tell them apart. The fix is worth doing largely *because* it retires that collision.
> - **Two divergences worse than `eqv` were missed.** `[Nil,].elems` is `0` in mutsu and `1` in raku —
>   `exec_make_array_no_flatten_op` (`src/vm/vm_data_ops.rs:160`) *drops* a `Nil` element outright, so
>   this is data loss, not a wrong value. And `my %h{Int} = 1 => Nil` **dies** in mutsu while raku
>   accepts it — the mirror image of the typed-array leniency this file records, from the same root.
>
> The ADR's §1.3 table (29 rows) and §1.4 invariant table (13 rows) supersede the repro notes below;
> the bisection history from RSV is kept here because the ADR does not carry it.

Supersedes `todo/tickets/rsv-from-rsv-result-extra-itemization-sigil.md`. That
ticket's own framing ("container-identity mismatch... extra itemization
sigil") was a **misdiagnosis** — the `got: $[[Any],]` vs `expected: [[Any],]`
message text is a red herring from `is-deeply`'s failure-report formatting,
not the actual defect. The real bug is upstream of any itemization/sigil
concern: mutsu's `[...]` array-literal construction does not decay a literal
`Nil` element to `Any` at construction time, while real Rakudo does — and
this leaks into `eqv`, sub-argument passing, and a separate typed-array
type-check divergence.

## Minimal repro (fully reduced from the original dist failure)

```raku
my @b = [Nil];
say @b eqv [Nil];   # rakudo: True    mutsu: False
```

Traced from `RSV.rakumod`'s `from-rsv` (`~/.cache/mutsu-dist-sweep/R_SV_RSV_*.tar.gz`)
down to this one-liner via ~15 rounds of bisection (see session history
2026-08-18) — every intermediate layer (`.kv` destructuring, `for @cases`,
`Test`'s own `is-deeply`, the `push`-vs-literal shape) turned out to be
irrelevant; only the `@`-sigil COPY-ASSIGNMENT (`my @b = [Nil]`, not `:=`
binding, not `$scalar = [Nil]`) mattered, and only in comparison to a
**freshly constructed** `[Nil]` (not a variable already holding the same
literal — see below for why).

## Root cause

`vm_var_assign_set_local.rs`'s `exec_set_local_op_inner` (`my @b = [Nil]`
codegen) has an explicit, narrow fixup for exactly this case
(`src/vm/vm_var_assign_set_local.rs:973-985`, and a byte-identical sibling for
the `AssignExpr` form at `src/vm/vm_var_assign_local.rs:156-170`):

```rust
// An untyped `@` assignment resets Nil elements to Any (their
// fresh containers' default; `my @a = (1,2)[1,2]` is `[2, Any]`).
if !is_bind
    && loan_env!(self, var_type_constraint(name)).is_none()
    && let ValueView::Array(items, kind) = assigned.view()
    && kind.is_real_array()
    && items.iter().any(Value::is_nil)
{
    ...
    *data.items_mut() = crate::runtime::utils::nil_elems_to_any(old_items);
    ...
}
```

This is confirmed (via `rust-gdb` breakpoints, not guesswork — see CLAUDE.md's
debugging guidance) to fire correctly for `my @b = [Nil]`: `@b`'s stored
element genuinely becomes `Value::package(Symbol::intern("Any"))` after this
runs. **The bug is that this is the ONLY place in the whole interpreter that
performs this decay.** A **freshly evaluated** `[Nil]` array-literal
expression — via `OpCode::MakeRealArray` → `exec_make_array_op`
(`src/vm/vm_data_ops.rs:4`) — never decays its `Nil` elements at all; the
function has zero `is_nil`/`Nil` handling. So:

- `@b eqv [Nil]`: LHS is `@b`'s (correctly decayed) `Package("Any")` element;
  RHS is the freshly-built literal's still-raw `ValueView::Nil` element.
  `Value::eqv` (`src/value/types_eqv.rs`) has no `(Nil, Package("Any"))` arm —
  correctly so, since **`Nil eqv Any` is `False` in real Rakudo too**
  (verified: `raku -e 'say Nil eqv Any'` → `False`). The mismatch isn't a
  missing `eqv` rule; it's that mutsu's two operands disagree on what a
  literal `[Nil]` even contains, when real Rakudo's never differ.
- `my @a := [Nil]` (binding, `is_bind` true) skips the fixup by design
  ("Binds keep the source values untouched") — so `@a`'s element stays raw
  `Nil`, which happens to match a fresh `[Nil]` literal's ALSO-raw `Nil` on
  the other side of `eqv`, so `is-deeply @a, [Nil]` passes for the wrong
  reason (both sides equally undecayed, not both correctly decayed).
- `my $c = [Nil]` (scalar, no `@` sigil) never reaches the array-specific
  fixup at all (it only triggers on `name.starts_with('@')`) — same
  accidental pass.

**Confirmed against real Rakudo that construction-time decay is unconditional
(regardless of what the literal is later bound/assigned/passed into):**

```
$ raku -e 'my @a := [Nil]; say @a.raku'          # [Any]  (binding doesn't preserve Nil)
$ raku -e 'sub f(@x) { say @x[0].WHAT }; f([Nil])'   # (Any) (sub-arg binding doesn't preserve Nil)
```

mutsu currently shows `Nil` (not `(Any)`) for the second case — direct proof
the leak isn't limited to `eqv`; any code path that inspects a `[Nil]`
literal's element WITHOUT going through the one narrow `@`-copy-assignment
fixup sees the wrong (undecayed) value.

## Second divergence discovered en route: mutsu invents a typed-array Nil→element-type coercion Rakudo doesn't have

While confirming the "decay is unconditional" claim, found mutsu is actually
**more lenient** than Rakudo in a way that depends on the very same
(incorrect) "Nil survives in typed-array elements for downstream coercion"
assumption (`vm_var_assign_set_local.rs`'s own comment: "Typed arrays keep
Nil here — the typed element coercion below converts it to the element type
object instead"):

```
$ raku -e 'my Int @a = [Nil]; say @a.raku'
Type check failed for an element of @a; expected Int but got Any (Any)
$ mutsu -e 'my Int @a = [Nil]; say @a.raku'
Array[Int].new(Int)          # succeeds — should be a type-check death
```

Real Rakudo does NOT special-case a literal `Nil` array element for typed
arrays at all — it's already `Any` by the time the type check runs, and
`Any` simply fails an `Int` constraint like any other wrong-type value.
mutsu's typed-array coercion path treats raw `Nil` as "please give me this
element's default," a feature Rakudo doesn't have here. **This means the
naive fix — "just decay `Nil→Any` universally at `MakeRealArray`
construction time" — would likely flip this second bug from
silently-wrong-but-passing to a NEW test failure** (mutsu would then also
type-error on `my Int @a = [Nil]`), so whoever picks this up must audit
`t/`/roast coverage for typed-array-with-Nil-literal cases before landing the
construction-time-decay fix, not just cargo-cult it in.

## Why this is `todo/deep`, not a `todo/tickets` slice

- The correct architectural fix (decay at `[...]` **construction** time,
  matching Rakudo, and DELETE the two narrow post-hoc `@`-assignment fixups
  as redundant) touches `exec_make_array_op` (`src/vm/vm_data_ops.rs`) — a
  function on the hot path for essentially every array literal in the
  interpreter. A change there is high-blast-radius by nature.
- It has a **known second casualty** (the typed-array leniency above) that
  needs its own decision: is mutsu's current typed-array Nil-coercion
  behavior worth preserving as a deliberate raku-compatible *extension*
  (unlikely, since it's not spec'd anywhere and roast presumably expects the
  type-check death), or was it only ever a side effect of working around
  this exact bug and should be deleted alongside the fix? Needs a roast run
  to see which whitelisted tests (if any) depend on the current behavior.
- Removing the two existing narrow fixups without a full audit risks
  reintroducing whatever they were originally added for (their own comments
  reference `my @a = (1,2)[1,2]` producing `[2, Any]` — a case worth
  re-verifying still works post-fix).
- Binding (`:=`) currently "accidentally passes" every test in this area by
  being equally wrong on both sides. **The open TODO here is now answered, and
  the answer removes the worry:** `raku -e 'my @a := [Nil]; say @a[0].WHAT'`
  is `(Any)`, because the `[Nil]` literal decayed at *its own* construction
  before the bind ever saw it. Rakudo has no "no-decay-on-bind" carve-out to
  match — binding is simply downstream of a decay that already happened. So
  construction-time decay needs no bind-side exception, and the existing
  `!is_bind` gate in the assignment fixup is itself a symptom of applying the
  rule at the wrong place.

## Severity

Low-to-moderate: a real semantic gap (any code comparing/inspecting a
`Nil`-containing array literal outside the one narrow assignment path sees
inconsistent results), but narrow in practical surface — most user code
never round-trips a bare `Nil` array element through `eqv`. The RSV dist
hit it specifically because `from-rsv` genuinely produces `Nil` elements as
part of its normal decode logic.

Affected: `src/vm/vm_data_ops.rs` (`exec_make_array_op`, the real fix site),
`src/vm/vm_var_assign_set_local.rs` / `src/vm/vm_var_assign_local.rs` (the
two existing narrow fixups, likely deletable once construction-time decay
lands), `src/value/types_eqv.rs` (`Value::eqv`, where the symptom surfaces),
`src/runtime/utils/coerce_containers.rs` (`nil_elems_to_any`, the existing
helper — reusable, just needs a new call site).

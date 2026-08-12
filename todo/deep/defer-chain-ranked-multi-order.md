# Deferral chain order for multi methods diverges from raku: ranked merged candidate list, not (MRO level, decl order)

Found by the ADR-0019 E9-pre raku verification campaign (2026-08-12, Rakudo v2026.06). This is
the highest-impact finding of the campaign: it invalidates a stated assumption of E9's cursor
design (design decision 2 in `todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md` says the
cursor should match "today's 'remaining = signature-matching candidates in MRO order' semantics"
— but today's mutsu semantics are themselves wrong for multis; the cursor must implement raku's
model instead).

## The divergence

When multi candidates for one method name exist at MORE THAN ONE MRO level, raku's
`nextsame`/`callsame` deferral order is the **globally ranked (specificity-sorted) merged
candidate list**, with MRO position only as a tie-break — NOT mutsu's "walk MRO levels outward,
declaration order within a level, filtered by signature match".

Minimal repro (raku left, mutsu right):

```raku
class P3 { multi method m(Int $x) { say "P3:Int"; "p3-int" } }
class C3 is P3 {
    multi method m(Int $x) { say "C3:Int"; nextsame; say "C3:unreached" }
    multi method m(Any $x) { say "C3:Any"; my $r = callsame; say "got({$r // 'Nil'})"; "c3-any" }
}
say C3.new.m(1);
# raku:  C3:Int -> P3:Int -> "p3-int"        (both Int candidates outrank C3:Any)
# mutsu: C3:Int -> C3:Any -> P3:Int -> "c3-any"  (level 0 exhausted first)
```

## The raku model (confirmed by probes, all consistent)

Method dispatch/deferral is **two-level**:

1. An **outer chain** of per-class entries found along the MRO: each entry is the method object
   installed at that class — either a plain method or a proto (explicit or implicit).
2. When a class declares `multi method` without its own explicit proto, the **implicit proto
   clones the nearest proto found in the MRO** and merges the parent's candidates with its own;
   the merged list is **ranked by narrowness** (specificity), MRO order breaking ties.
   `nextsame`/`callsame` from a multi candidate first walk the REST OF THAT RANKED LIST; only
   when it is exhausted do they fall to the outer chain's next entry (the next per-class entry
   AFTER the proto's own class).

Consequences verified by probe (`tmp/e9pre/probe-mix.raku`, `probe-mix2.raku` during the
campaign):

- Both-levels-multi: chain is `[C:Int, P:Int, C:Any]` (ranked), not `[C:Int, C:Any, P:Int]`.
- Plain method in the MIDDLE of the MRO (`A2` has `multi m(Int)`, `B2 is A2` has plain `m`,
  `C2b is B2` has `multi m(Int)`): raku runs `C2b:Int -> A2:Int -> B2::m -> A2:Int (again) ->
  Nil`. The child's implicit proto merged A2's candidate (skipping over B2's plain entry, which
  is not a multi candidate), so A2:Int is reached FIRST via the ranked list; exhausting the list
  falls to the outer chain entry after C2b, which is B2's plain method; B2::m's own callsame then
  starts the walk BELOW B2, re-entering A2's proto — so **A2:Int legitimately runs twice**.
  mutsu runs `C2b:Int -> B2::m -> A2:Int -> Nil` (strict MRO interleave, no re-visit).
- Cases where the two models agree (single level; multi child + plain parent; plain child +
  parent multi set; all-plain chains) are pinned by `t/defer-multi-single-class.t`,
  `t/defer-multi-plain-cross-level.t`, `t/defer-inherited-chain.t`. The DIVERGING shapes are
  deliberately NOT pinned yet — the pin lands with the fix.

Related same-model findings, ticketed separately: an explicit child proto must NOT assume parent
candidates (`todo/tickets/explicit-child-proto-assumes-parent-candidates.md`), and a
class-overridden `does`-composed role method must not appear in the chain at all
(`todo/tickets/role-shadowed-method-in-defer-chain.md`).

## Affected mutsu code

- `resolve_all_methods_with_owner` (`src/runtime/resolution_method.rs`) — the deferral-list
  walker: walks MRO levels outward, declaration order within a level, signature-filtered. This
  produces the wrong order whenever multi candidates span levels.
- `push_method_dispatch_frame` (`src/runtime/accessors_state.rs`) and the
  `MethodDispatchFrame.remaining` consumers (`src/runtime/builtins_dispatch_next.rs`).
- E9's planned `DispatchCursor` (design decision 2) must build its sequence in the raku order:
  the resolver's `ResolvedSequence` (E4) currently stores candidates in `(level, stored_idx)`
  order — the cursor needs the RANKED order for the multi portion plus outer-chain fall-through
  and the re-visit semantics for plain middle entries.

## Why this is deep

It is not a local fix: the deferral order is the load-bearing semantic E9a/E9b/E9c will encode
into the cursor. Doing it inside today's `resolve_all_methods_with_owner` means re-implementing
ranking in a walker E9 plans to delete; doing it in E9 means the cursor's sequence layout (one
flat list) must grow the two-level structure (ranked multi block + outer chain + re-entry
point). Either way it needs design alignment with
`todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md` first — that doc's decision 2 should
be amended before E9a starts.

**Update (same day): the amendment landed, and the "two-level structure" concern above is
resolved** — the "E9 design decision 2 — REDRAWN" section of
`todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md` records a FLAT expansion (per-class
entries, each a plain method or a proto's ranked block, duplicates across blocks allowed) that
reproduces raku exactly, confirmed by two exact-hit predictions (probe 1: parent candidate runs
twice via block re-visit; probe 2: three-level implicit-clone chain). A flat cursor index
suffices; only the sequence BUILDER changes. This ticket remains open as the tracking item for
the actual behavior fix, which is E9a's cutover.

## Repro harness

Campaign probes lived in `tmp/e9pre/` (gitignored); the two order-diverging scripts are inlined
above and in the E9-pre progress note in
`todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md`.

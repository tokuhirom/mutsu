# Deferral chain order for multi methods now matches raku's ranked merged candidate list

ADR-0019 E9-pre (the raku ground-truth verification campaign for
`samewith`/`nextsame`/`callsame`/`nextwith` cursor semantics) found that when multi candidates
for one method name exist at more than one MRO level, raku's `nextsame`/`callsame` deferral
order is the **globally ranked (specificity-sorted) merged candidate list**, with MRO position
only as a tie-break — not mutsu's previous "walk MRO levels outward, declaration order within a
level, filtered by signature match".

Minimal repro:

```raku
class P3 { multi method m(Int $x) { say "P3:Int"; "p3-int" } }
class C3 is P3 {
    multi method m(Int $x) { say "C3:Int"; nextsame; say "C3:unreached" }
    multi method m(Any $x) { say "C3:Any"; my $r = callsame; say "got({$r // 'Nil'})"; "c3-any" }
}
say C3.new.m(1);
# raku:            C3:Int -> P3:Int -> "p3-int"        (both Int candidates outrank C3:Any)
# mutsu (before):  C3:Int -> C3:Any -> P3:Int -> "c3-any"  (level 0 exhausted first)
```

## The raku model

Method dispatch/deferral is two-level:

1. An **outer chain** of per-class entries found along the MRO: each entry is the method object
   installed at that class — either a plain method or a proto (explicit or implicit).
2. When a class declares `multi method` without its own explicit proto, the **implicit proto
   clones the nearest proto found in the MRO** and merges the parent's candidates with its own;
   the merged list is **ranked by narrowness** (specificity), MRO order breaking ties.
   `nextsame`/`callsame` from a multi candidate first walk the rest of that ranked list; only
   when it is exhausted do they fall to the outer chain's next entry.

This also allows a plain method in the middle of the MRO to be skipped by the ranked merge and
reached later via the outer chain — meaning a parent multi candidate can legitimately run twice
in one call (once via the ranked block, once via re-entering the proto below the middle plain
method).

## The fix

`Interpreter::resolve_deferral_expansion` (`src/runtime/resolution_deferral.rs`) is a new
ordering source that replaces `resolve_all_methods_with_owner` at the two "remaining"-building
call sites (`accessors_state.rs::push_method_dispatch_frame`, `class_dispatch.rs`'s
`build_remaining` closure): it builds the flat per-MRO-class expansion described above
(implicit-clone-merge ranked by nominal narrowness/MRO-depth/decl-order, explicit-proto
isolation) instead of a bare per-level declaration-order walk.

The winner-removal mechanism (fingerprint-compare-and-skip) is unchanged — only its input
ordering changed. The full `DispatchCursor{seq, next, invocant, args}` index-based refactor
(ADR-0019 design decision 2) is a separate, lower-risk follow-up left for a later slice; no
observable behavior depends on which storage shape is used.

Both probes from the campaign are exact hits against Rakudo v2026.06 and pinned in
`t/defer-multi-cross-level-proto-block.t`; all 12 E9-pre pins plus the full
`multi`/`nextsame`/`callsame`/`wrap`/`proto`/`defer`/`samewith` corner of `t/` (148 files) stay
green.

See also: `docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md` (ADR-0019 E9a),
`news/2026-08/adr0019-e8-e11-candidate-sequence-semantics.md` (design decision 2, redrawn).

# A `does`-composed role method overridden by the class must NOT be in the nextsame/callsame chain

Found by the ADR-0019 E9-pre raku verification campaign (2026-08-12, Rakudo v2026.06).

## Divergence

```raku
role R { method m() { say "R::m"; "r" } }
class C does R { method m() { say "C::m"; my $r = callsame; say "C-got({$r // 'Nil'})"; "c" } }
say C.new.m;
# raku:  C::m -> C-got(Nil) -> c        (role method is fully shadowed, NOT a chain entry)
# mutsu: C::m -> R::m -> C-got(r) -> c  (role's own copy is walked)
```

The same holds for a same-signature `multi method` pair (role's candidate is a dropped
flattened duplicate in raku — callsame from the class's candidate yields Nil, but mutsu reaches
`R2:Int`). A role candidate with a DIFFERENT (narrower) signature DOES participate in dispatch
normally (both implementations agree there), and a runtime mixin (`but R` / `does R` on an
instance) keeps its own MRO entry in both (pinned by `t/nextsame-role-mixin.t`,
`t/callsame-punned-role-and-hyper-infix-sub.t` — those cover pun/mixin shapes, which are NOT
affected by this ticket).

## Root cause

`resolve_all_methods_with_owner` (`src/runtime/resolution_method.rs`) reads
`registry().roles.get(cn)` directly for MRO entries that are roles, so a composed role's raw
(un-flattened) method lands in the deferral list even when the class's own method shadows it.
raku's chain contains only the flattened copy — which, when the class defines its own method of
the same name, is the class method alone.

Note the cross-link: `todo/deep/method-entries-never-covers-unpunned-roles.md` records that the
E8a sequence probe (reading `method_entries`) OMITS un-punned role methods and treats the real
walker as authoritative. This campaign's ground truth shows that for the shadowed-by-class
shape the real walker is the wrong side: the sequence's omission matches raku. The two must be
reconciled together — whichever store wins, the chain must exclude class-shadowed composed role
methods (while keeping qualified calls `self.R::m()` and role-conflict-resolution shapes
working; those read different paths).

## Fix sketch

Drop a role's raw MRO-level entry from the deferral walk when a class level in the chain
already carries a method of the same name originating from that role's flattening OR its own
override — i.e. apply the same rule `drop_flattened_role_duplicates` implements for winner
selection, plus the class-override shadow. Then re-run E8a's `MUTSU_VM_STATS=1` deferral-shadow
sweep: the 58 accepted mismatches attributed to the un-punned-role gap should be re-audited
against raku rather than assumed to be sequence-side omissions.

The E9-pre pin for this lands with the fix (currently no `t/` pin encodes either behavior).

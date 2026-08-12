# `lastcall` inside a method wrapper makes a following callsame die instead of returning Nil

Found by the ADR-0019 E9-pre raku verification campaign (2026-08-12, Rakudo v2026.06).

## Divergence

```raku
class C { method m() { say "orig"; "o" } }
C.^lookup('m').wrap(-> |c { say "wrap"; lastcall; my $r = callsame; say "after({$r // 'Nil'})"; "w" });
say C.new.m;
# raku:  wrap -> after(Nil) -> w      (lastcall empties the chain; callsame then yields Nil; orig never runs)
# mutsu: wrap -> dies "callsame is not in the dynamic scope of a dispatcher"
```

The non-wrap variant (lastcall then nextsame inside a plain multi candidate) behaves correctly
in both and is pinned by `t/lastcall-then-nextsame.t`.

## Root cause guess

`lastcall` truncates the topmost dispatch frame (`builtins_dispatch_next.rs:62-75` per the
E8-E11 survey). For a `WrapDispatchFrame` the truncation appears to pop/destroy the frame
itself rather than just emptying its `remaining`, so the subsequent `callsame` finds no
dispatcher frame at all and raises the out-of-scope error instead of resolving to an exhausted
chain (Nil).

## Fix route

Make lastcall-on-a-wrap-frame leave the frame in place with an emptied remaining list (both the
wrapper tail AND the `sub_id == 0` fall-through to the method leg must be cut — in raku the
original method does NOT run). ADR-0019 E9b (wrap frames folded into the DispatchCursor as
prefix entries; `lastcall` becomes `next = seq.len()`) fixes this structurally — if E9b is
close, fix it there and add the pin in the same PR.

The E9-pre pin for this lands with the fix.

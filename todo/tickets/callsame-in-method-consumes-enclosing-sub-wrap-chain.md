# callsame in a method dispatched from inside a sub's wrapper consumes the sub's wrap chain

Found by the ADR-0019 E9b design pass (2026-08-13, Rakudo v2026.06).
`dispatch_next_candidate` searches the deferral stacks in FIXED priority — wrap stack first,
then method stack (`builtins_dispatch_next.rs:403-406`) — so a live sub-wrap frame shadows a
more recently pushed method dispatch frame.

## Divergence

```raku
class P { method m() { "P-m" } }
class C is P { method m() { "C-m[" ~ callsame() ~ "]" } }
sub g() { "g-orig" }
&g.wrap(sub () { say C.new.m; "g-wrap[" ~ callsame() ~ "]" });
say g();
# raku:  C-m[P-m]    then g-wrap[g-orig]
# mutsu: C-m[g-orig] then "Use of Nil in string context" warning + g-wrap[]
```

The `callsame` inside `C.m` should walk C's MRO (reaching `P.m`), but mutsu finds `g`'s
wrap frame first and invokes `g`'s original instead; `g`'s own `callsame` then finds an
exhausted chain and gets Nil.

## Root cause

`builtin_callsame`/`nextsame`/`callwith`/`nextwith`/`lastcall`/`nextcallee` all pick their
frame by stack IDENTITY (wrap → method → multi), not by dispatch recency. Any program that
nests a method deferral inside a sub wrapper (or vice versa with a live multi frame) gets the
wrong frame.

## Fix

ADR-0019 E9b-0 (see the "E9b design" section, decision 4, of
`todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md`): stamp all three frame kinds with
a shared monotonic `dispatch_token: u64` at push and select the live frame with the highest
token — the innermost dynamic dispatch context — in `dispatch_next_candidate`,
`builtin_lastcall`, and `builtin_nextcallee`. For today's paired method-wrap frames the wrap
frame is pushed second and still wins, so only the cross-stack nesting shape changes. The
raku-valued pin for this probe lands with that slice.

# A `does`-composed role method overridden by the class is no longer a nextsame/callsame chain entry

Found by the ADR-0019 E9-pre raku verification campaign (2026-08-12, Rakudo v2026.06); fixed
2026-08-13 as the first follow-up slice after E9a landed.

## The bug

```raku
role R { method m() { say "R::m"; "r" } }
class C does R { method m() { say "C::m"; my $r = callsame; say "C-got({$r // 'Nil'})"; "c" } }
say C.new.m;
# raku:  C::m -> C-got(Nil) -> c        (role method is fully shadowed, NOT a chain entry)
# mutsu (before): C::m -> R::m -> C-got(r) -> c  (role's own copy was walked)
```

`resolve_all_methods_with_owner` and `resolve_deferral_expansion` (both in
`src/runtime/resolution_method.rs` / `resolution_deferral.rs`) read a composed role's raw
methods directly from `registry().roles` for any MRO level that is a role, so a role's own copy
of an overridden method landed in the `nextsame`/`callsame` deferral list even when the class's
own method fully shadowed it. `drop_flattened_role_duplicates` already dropped a role's raw
entry when a *flattened* copy (tagged `role_origin`) was present in the same match set, but
`resolve_class_stub_requirements` (`registration.rs`) removes that flattened copy from the
class's own method table whenever the class provides an independently-authored override with a
matching signature — so in the shadowed-by-class case there never was a flattened copy for the
old check to key off of, and the role's raw entry survived the walk untouched.

## The fix

`drop_flattened_role_duplicates` now also drops a role's raw entry when a *class-owned* method
of matching signature exists anywhere else in the same match set — the same "class wins over its
role" rule `resolve_class_stub_requirements` already applies to `class_def.methods`, just
applied a second time here at the deferral/winner-candidate-list level. A role candidate whose
signature genuinely differs (a narrower `multi` overload) keeps participating normally, matching
raku.

The tricky part was telling a `does`-composed role apart from a role used as a *punned* class
parent (`class Foo is R1`): raku's own `.^mro` puts a punned role in the real ancestor chain as a
genuine class (`(C2) (R2) (Any) (Mu)`), unlike a `does`-composed role, which never appears in
`.^mro` at all (`(C1) (Any) (Mu)`) — so overriding a punned role's method is ordinary
single-inheritance shadowing, and `nextsame` legitimately reaches the parent
(`t/callsame-punned-role-and-hyper-infix-sub.t`, an existing pin, caught this on the first
attempt). `install_role_puns` gives a punned role a real — if method-empty — `registry().classes`
entry, which is exactly the fact the fix reads to tell the two shapes apart: a role name present
in `registry().roles` but absent from `registry().classes` is a pure `does` composition and is
eligible for shadowing; a role name present in both is a punned parent and keeps its raw entry.

New pin: `t/role-shadowed-method-in-defer-chain.t` (plain method override, same-signature multi
pair, different-signature multi pair still dispatching, and a role-qualified `self.R::m()` call
still reaching the role directly).

## Scope

This does not touch `todo/deep/method-entries-never-covers-unpunned-roles.md` (the E1/E2
canonical `method_entries` table's separate, broader gap for un-punned roles) — that ticket
covers production dispatch call sites this fix does not touch, and stays open.
`todo/tickets/explicit-child-proto-assumes-parent-candidates.md` and
`todo/tickets/native-array-push-defer-fallback-broken.md`, the other two E9-pre findings, also
remain open.

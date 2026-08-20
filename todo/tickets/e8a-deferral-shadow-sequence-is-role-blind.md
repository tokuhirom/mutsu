# The E8a deferral shadow check compares a role-blind sequence against a role-aware real walker

**Status: ready for direct implementation, no design needed.** The policy question this sits on top
of was already decided by ADR-0019 F4a and F4c section (1); see
`news/2026-08/method-entries-never-covers-unpunned-roles.md` for the closeout of the deep finding
this residual came out of. Nothing here needs an ADR.

## What is wrong

`Interpreter::shadow_check_deferral_sequence` (`src/runtime/resolution_sequence.rs`) compares the
real `nextsame`/`callsame` "remaining" list against a shadow list built from
`Interpreter::resolve_sequence`. The two sides no longer read the same candidate universe:

- the real side is `Interpreter::resolve_deferral_expansion`
  (`src/runtime/resolution_deferral.rs`, ADR-0019 E9a), whose per-level lookup
  `own_overloads_at_level` deliberately uses `Registry::get_method_overloads_with_role_fallback`;
- the shadow side is `resolve_sequence` (`resolution_sequence.rs`, the per-level
  `registry().user_method_overloads(owner_str, name.as_str())` call), which has no role fallback
  and therefore silently omits every candidate owned by a role that was never `.new`-punned.

So every deferral through a role-in-MRO shape is reported as a mismatch even though production
dispatch is correct. Confirmed on `main` (2026-08-20) with a `MUTSU_VM_STATS=1` sweep of `t/`; the
mismatches all carry the same fingerprint, `real_len = shadow_len + 1` with the missing candidate
role-owned:

```
t/callsame-punned-role-and-hyper-infix-sub.t  class=Foo method=foo real_len=1 shadow_len=0
                                              class=Bar method=foo real_len=1 shadow_len=0
t/anon-class-does-imported-role.t             class=__ANON_CLASS_0..3__ method=search real_len=1 shadow_len=0
t/cro-http-battery.t                          class=Cro::HTTP::Router::RouteSet::RouteHandler method=signature real_len=1 shadow_len=0
                                              class=Cro::HTTP::BodySerializer::JSON method=is-applicable real_len=1 shadow_len=0
                                              (and three more of the same shape)
```

`t/callsame-punned-role-and-hyper-infix-sub.t` is the clearest minimal case: `my role R1 { method
foo {...} }; my class Foo is R1 { method foo { ... callsame } }` — R1 sits in Foo's MRO under its
own name and is never punned, so the real walker finds `R1::foo` through the fallback and the
shadow finds nothing.

## The fix

`resolve_sequence` is **also** the candidate source for live winner selection
(`resolve_via_sequence_cache`, called from `vm/vm_call_method_compiled_cache.rs`), and ADR-0019
F4a's rule is explicit that winner selection must never consult the role fallback. So do **not**
simply swap the lookup in `resolve_sequence`. Give the function a role-fallback mode instead — a
small enum/bool parameter, threaded from `shadow_check_deferral_sequence`'s own
`resolve_sequence(...)` call only, with every other caller (including the cached winner-selection
path, whose `resolved_seq_cache` key must keep meaning "no fallback") passing the current
role-blind behaviour. That is the whole change; the surrounding shadow-comparison logic is
unaffected.

## Also in scope: the stale module doc

`src/runtime/resolution_sequence.rs`'s module doc, the "E8a's own accepted divergence" paragraph,
is now wrong in three separate ways and should be rewritten with the fix:

- it explains the gap through `Registry::method_entries` being "only ever populated for
  `self.classes` keys" via `sync_user_method_entries` — that function was deleted by ADR-0019
  F4c-9b; the table is now maintained by `registry_method_table.rs`'s per-declaration mutators, and
  role exclusion is stated policy (F4c design note section (1)), not an accident of a writer;
- it names `resolve_all_methods_with_owner` as "the real deferral-list walker
  `push_method_dispatch_frame` still uses" — E9a replaced that with `resolve_deferral_expansion`;
- it points at `todo/deep/method-entries-never-covers-unpunned-roles.md` for the fix plan, which
  has been closed out to `news/2026-08/method-entries-never-covers-unpunned-roles.md`.

`src/runtime/registry.rs`'s `role_method_overloads` doc comment carries the same dead path and
should be repointed at the same time.

## Note: fixing this will not take the mismatch count to zero

A second, unrelated divergence class exists and is out of this ticket's scope. The shadow builder
walks the MRO per level in stored declaration order, while E9a's real expansion builds *governing
proto blocks* (a class's own multi candidates merged with the nearest ancestor's block, ranked
nominally). `t/defer-multi-cross-level-proto-block.t` — which contains no roles at all — reports
`class=C method=m real_len=3 shadow_len=2` and `class=Ch method=m real_len=2 shadow_len=0` purely
from that ordering difference. Whoever closes this ticket should expect the role-shaped mismatches
to vanish and the proto-block-shaped ones to remain, and should file the second class separately if
E8a's "drive the mismatch count to zero" goal is still wanted.

## Repro

```
cargo build
MUTSU_VM_STATS=1 ./target/debug/mutsu t/callsame-punned-role-and-hyper-infix-sub.t 2>&1 | grep adr0019-e8a
```

Expect `deferral_shadow_checks=2 deferral_shadow_mismatches=2` today, and
`deferral_shadow_mismatches=0` once the fallback is threaded through.

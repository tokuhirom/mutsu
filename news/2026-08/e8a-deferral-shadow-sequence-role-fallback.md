# The E8a deferral shadow check no longer misreports every role-in-MRO deferral as a mismatch

`Interpreter::shadow_check_deferral_sequence` (`src/runtime/resolution_sequence.rs`) compares the
real `nextsame`/`callsame` "remaining" list against a shadow list built from
`Interpreter::resolve_sequence`. The two sides used to read different candidate universes:

- the real side, `Interpreter::resolve_deferral_expansion` (`src/runtime/resolution_deferral.rs`,
  ADR-0019 E9a), consults `Registry::get_method_overloads_with_role_fallback` at each MRO level;
- the shadow side, `resolve_sequence`, called only the plain `Registry::user_method_overloads`,
  which has no role fallback and silently omits every candidate owned by a role that was never
  `.new`-punned.

So every deferral through a role-in-MRO shape (`my role R1 { method foo {...} }; my class Foo is R1
{ method foo { ... callsame } }`) was reported as a shadow mismatch even though production dispatch
was correct all along.

## The fix

`resolve_sequence` gained a `RoleFallback` parameter (`Enabled` / `Disabled`). It matters because
`resolve_sequence` is *also* the candidate source for live winner selection
(`resolve_via_sequence_cache`), and ADR-0019 F4a's rule is that winner selection must never consult
the role fallback — so the lookup could not simply be switched globally. Only
`shadow_check_deferral_sequence`'s own call site passes `RoleFallback::Enabled`; every other caller
(including `resolve_via_sequence_cache`, whose `resolved_seq_cache` key must keep meaning "no
fallback") passes `RoleFallback::Disabled`, the original role-blind behavior, unchanged.

Verified via `MUTSU_VM_STATS=1` on `t/callsame-punned-role-and-hyper-infix-sub.t`:
`deferral_shadow_mismatches` goes from 2 to 0.

## What is still open

A second, unrelated divergence class exists and remains out of scope: the shadow builder walks the
MRO per level in stored declaration order, while E9a's real expansion builds *governing proto
blocks* (a class's own multi candidates merged with the nearest ancestor's block, ranked nominally).
`t/defer-multi-cross-level-proto-block.t` — which contains no roles at all — still reports
`class=C method=m real_len=3 shadow_len=2` and `class=Ch method=m real_len=2 shadow_len=0` purely
from that ordering difference. This would need its own investigation if the E8a shadow check's
mismatch count is to reach zero.

`resolution_sequence.rs`'s module doc was also rewritten: it previously explained the gap through
`Registry::method_entries` being "only ever populated for `self.classes` keys" via
`sync_user_method_entries` (deleted by ADR-0019 F4c-9b — role exclusion from `method_entries` is
stated policy now, not an accidental side effect of a writer), and named
`resolve_all_methods_with_owner` as the real deferral-list walker (superseded by
`resolve_deferral_expansion`, E9a).

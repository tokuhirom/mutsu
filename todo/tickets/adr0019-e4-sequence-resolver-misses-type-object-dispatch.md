# `resolve_sequence` (ADR-0019 E4) misses methods reachable via `run_instance_method` on a type-object receiver

Found by ADR-0019 F6's "instanceops" shadow-check tagging (see the design note's F6 progress
notes), gathering corpus evidence before migrating `methods_instance_ops.rs`'s package/type-object
dispatch call site off `run_instance_method`.

## Repro

```raku
role NotNewPun {
    method x { 69 }
}
say NotNewPun.x;   # 69 -- a role type object puns for a non-`.new` method
```

`t/role-instantiation.t` and `t/role-pun-dispatches-on-type-object.t` already exercise this shape.
Also seen with `t/nested-type-short-name-owner-scope.t` (`Elsewhere::Header.tag` — a qualified,
nested-package type object) and a `role R { multi method COERCE {...} }` type-object coercion call.

## What's happening

`methods_instance_ops.rs`'s "Package (type object) dispatch" branch (~line 1658, tagged
`"instanceops"` for shadow-checking) resolves these calls correctly via `has_user_method` +
`run_instance_method`'s ad-hoc `resolve_method_with_owner_invocant` walk. The `MUTSU_VM_STATS`-gated
shadow probe added alongside it compares that answer against the modern
`resolution_sequence::resolve_sequence` resolver (the E4 candidate-sequence builder) for the same
`(chain, method, NativeCallShape)` and finds it comes back empty:

```
[mutsu vm-stats] adr0019-e4a resolver-shadow mismatches by site: instanceops
  [class=NotNewPun method=x real=Some("NotNewPun") shadow=None]
```

A full local `t/` sweep (3187 files, `MUTSU_VM_STATS=1`) found 9 such mismatches, all `real=Some(...)
shadow=None` (the ad-hoc walk finds the method, the sequence resolver does not) — never the reverse.
All 9 involve a `Package`/type-object receiver (`dispatch_mro`'s invocant is
`Value::package(Symbol::intern(receiver_class_name))`, `Definedness::TypeObject`), not a live
instance. The `vm_stats.rs` module doc already flags one related, deliberately-accepted gap
(`resolve_sequence` not modeling `resolve_method_with_owner_impl`'s "a non-multi method resolves by
name alone, independent of whether the call's arguments actually bind it" early-stopping rule) — this
may be the same root cause manifesting for type-object receivers specifically (a bare, no-`:D`/`:U`
method should be name-resolvable on a type object same as an instance), or a distinct definedness-
filtering gap in `method_args_match_for_invocant` / `NativeCallShape` construction. Not yet
root-caused to a specific line.

## Why this matters, and why it isn't fixed here

This is currently harmless: the shadow check is purely diagnostic (`record_resolver_shadow_check`),
nothing consults it to make a real dispatch decision, so production behavior is unaffected. But it
means **`methods_instance_ops.rs`'s Package-dispatch call site cannot be migrated from
`run_instance_method` to the sequence resolver** (ADR-0019 F6's eventual goal for this family) until
this gap is closed — the sequence resolver would silently fail every `SomeType.role_pun_method` /
`SomeQualifiedType.method` call that currently works. Root-causing requires reading
`resolve_sequence`/`method_args_match_for_invocant` closely for how they treat a `TypeObject`
invocant, which is its own, separate investigation from F6's tagging slice — filed here rather than
bundled into that PR, per this repo's "fix what you can, record the rest, move on" rule for
multi-prerequisite work.

## Suggested next step

Read `resolution_sequence.rs`'s `resolve_sequence` and `method_args_match_for_invocant` for how they
handle a `Definedness::TypeObject` invocant with a plain (non-`:D`/`:U`-constrained) method
candidate, using `NotNewPun.x` as the minimal repro. Compare against
`resolve_method_with_owner_impl`'s corresponding logic in `resolution_method.rs` to find exactly
where the two diverge.

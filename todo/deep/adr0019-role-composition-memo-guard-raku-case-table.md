# Role composition once-per-composition memoization: unguarded class-composition path, needs a raku case table

Spun off from `todo/deep/adr0019-d7-d8-role-plan-encoding.md` (ADR-0019 D7/D8, both now closed)
before that design doc is retired, since this item was explicitly deferred there as "V2" and
never independently filed.

## What the D7/D8 survey found

A role's deferred body (the statements in a role body other than attributes/methods — `use`,
side-effecting code, nested type decls, ...) executes once per composition via
`run_block_raw`/its D8-1 chunk successor. Today's once-per-composition tracking is **partial**:

- `Registry::composed_role_bodies` memoizes `pun:{role}` and `mixin:{role}` — role-global keys,
  not per-target. Punning or mixing the same role twice runs the deferred body only once, no
  matter which value/class it lands on.
- The **class-composition path has no guard at all** — `class A does R { ... }` re-registering
  the same class (e.g. inside a loop, or via re-`EVAL`) re-runs `R`'s deferred body every time.

D7/D8 deliberately preserved this exact behavior (not a regression from the chunk migration) —
the ADR box's own text says "correct once-per-composition behavior" and the design doc reads that
as "keep today's observable behavior while making each run cheap," explicitly declining to change
semantics as part of the bytecode migration.

## Why this needs its own ticket

Whether the memo *should* be `(role, target)`-keyed (matching real Rakudo) is a raku-conformance
question independent of the chunk migration, and nobody has actually checked what real Rakudo
does across the cases that would distinguish "role-global" from "per-target" from "unguarded"
memoization. Before writing any fix, build a small case table against `raku` covering (at least):

1. A loop that redeclares the same class name, each iteration composing a role with a
   side-effecting deferred body (`role R { say "composed" }` composed N times via a loop) — does
   Rakudo re-run the body every iteration, or only once ever?
2. The same role mixed into two different values via `does`/`but` at runtime — does each mixin
   target get its own run, or is it memoized globally per role?
3. Two distinct classes each composing the same role — does each class get its own run (current
   mutsu behavior for the class path, since there's no guard) or is it memoized across classes?

## Where to look

- `Registry::composed_role_bodies` (role-global memo map)
- `registration_class_compose_body.rs:64-277` (`run_composed_role_deferred_body`, the unguarded
  class-composition path)
- `registration_class_augment.rs:1258-1303` (`run_role_body_for_composition`, the shared runner
  also used by puns/mixins)

Once the case table exists, file any divergence from `raku` as its own fix ticket rather than
folding a behavior change into a bytecode-migration box.

# Role composition memoization: a raku case table, and two real divergences fixed

`todo/deep/adr0019-role-composition-memo-guard-raku-case-table.md` asked for a case table
against real `raku` to settle whether a role's deferred (non-declaration) body should be
memoized per role, per (role, target), or not at all across three composition paths: a class
declaration re-executed for the same class name (e.g. inside a loop), two distinct classes
composing the same role, and a runtime `does`/`but` mixin applied to one or more values. The
ticket deliberately left the answer open — this had never been checked against Rakudo.

## The case table

All scenarios used a role with a genuine method (a bare, method-less role composed via a
literal value gets constant-folded and composed at Raku *compile* time, which would have
confounded the count — see "Raku quirks found along the way" below) and a side-effecting
statement in its deferred body, verified with `raku` 2026.06 (Rakudo, MoarVM):

| # | Scenario | Rakudo | mutsu before | mutsu after |
|---|----------|--------|---------------|--------------|
| 1 | `class A does R {}` re-declared inside a runtime loop (same class name each pass) | body runs **once** | body ran on **every pass** | once |
| 2 | Two distinct classes `class A does R {}; class B does R {}` | body runs **once per class** (twice total) | twice (already correct) | twice |
| 3 | `1 but R; 2 but R` — `but` twice on the same base type | body runs **once** (memoized) | once (already correct) | once |
| 4 | `1 but R; "x" but R` — `but` on two *different* base types | body runs **once per base type** (twice total) | once (wrong — over-memoized) | twice |
| 5 | `$obj does R` on a single instance | body runs **once** | **twice** (a duplicate direct call plus the class-composition path both ran it) | once |
| 6 | `does` on two instances of the *same* class | body runs **once** | twice (same duplicate-call bug) | once |
| 7 | `does` on instances of two *different* classes | body runs **once per class** (twice total) | three times (duplicate call plus per-name-mismatch double count) | twice |

The unifying rule Rakudo actually implements: **a role's deferred body runs once per (role,
resulting composed TYPE)** — not once per role globally, and not once per call site. For a
class-header `does`, the composed type is the class itself, so distinct classes each get
their own run (case 2) but the same class name re-executed does not (case 1). For a value
`but`/`does`, Rakudo builds (and caches) an anonymous type per (base type, role) pair — an
`Int+{R}` is a different type from a `Str+{R}` — so mixing the same role onto two different
base types runs the body twice (case 4) while two values of the *same* base type share the
one cached composed type and only run it once (case 3, case 6).

## What was fixed

- `Registry::composed_role_bodies` already memoized `pun:{role}` and `mixin:{role}`, but both
  keys were **role-global**, ignoring the target's base type. Fixed the `mixin:` key (used by
  `but`/`does` on a non-`Instance` value, `runtime/types/roles.rs`'s `compose_role_on_value`)
  to include `crate::value::types::what_type_name(&left)` — Rakudo's own naming scheme
  (`Int+{R}` vs `Str+{R}`) doubles as the right memo key.
- The **class-composition path** (`register_class_decl` → `compose_class_parent_roles` →
  `compose_role_into_class`, `runtime/registration_class_compose.rs`) had **no guard at all**:
  every call re-ran the role's deferred body. Added a `class:{class_name}:{resolved_role_name}`
  key so the same class re-declaring the same role composition is idempotent, while two
  distinct classes each still get their own run.
- Found and removed a **duplicate execution bug** on the `does`-on-`Instance` path
  (`runtime/types/role_mixin_class.rs`'s `does_rebless_instance`): it called a standalone
  `run_mixin_role_body` (the role-global `mixin:` memo) *and* `ensure_mixin_class`, which
  itself composes the role into a synthesized `Base+{R}` class via the (now-guarded)
  class-composition path — running the same deferred body twice for a single `does`. Removed
  the redundant direct call; the class-composition path alone now runs it exactly once per
  synthesized mixin type, which is also correctly keyed per base type (case 7).
- The class-composition guard needed two more corrections once wired up, both caught by
  existing tests:
  - **Error rollback.** A dying deferred body (a parameterization guard,
    `role Guarded[::T] { die unless ... }`) must reject *every* retry, not just the first. The
    memo key is inserted before running the body (to guarantee at-most-once on success), so a
    failed attempt now removes its own key again — otherwise a second `.new` on the same
    rejected parameterization silently succeeded instead of re-dying
    (`t/role-body-guard-parameterisation.t`).
  - **Hoisted-shell exemption.** A class declaration that follows a genuine runtime statement
    in its block (`hoist_type_decl_shells`'s condition — the common case for any class not at
    the very top of a file) first gets a throwaway `__hoisted` forward-reference shell
    registration, superseded at runtime by the real, source-position declaration. Getting this
    exemption right took three iterations:
    1. No exemption (the memo key applied uniformly): the shell's run consumed the
       class-composition memo key, permanently starving the real declaration's run
       (`t/run-nested-role-body.t`'s `$side = @outer.elems * 100` caught this — `$side` never
       got set, because the shell's registration-time env is transient and never reaches the
       mainline's env the real declaration writes into).
    2. Skip the shell's deferred-body run entirely, reasoning that it has no
       forward-reference need for arbitrary side-effecting code (only for the
       methods/attributes the unconditional copy above the guard already handles): broke
       `t/indirect-declarator-names.t`, whose `role RIndirect { my constant rname = 'rsecond';
       method ::(rname) { ... } }` needs the constant to actually be *bound* during the
       shell's own registration for the shell's copy of the indirectly-named method to resolve
       `rname` — a role's deferred body is not purely user-observable side effects, it can also
       declare structural state (a `constant`, a nested type) a later statement in the SAME
       registration pass needs.
    3. The fix that stuck: the shell runs the deferred body **unconditionally**, exactly as
       pre-guard mutsu always did, and is simply exempt from ever touching the
       class-composition memo (so it can never block or be blocked by the real pass). A
       registry-level declaration from the shell's run (like that `constant`) genuinely
       persists and is what makes indirect method naming work; a mainline-lexical write from
       the shell's run does not persist (case in point: `run-nested-role-body.t`'s `$side`).
       But a *global* write from the shell's run — visible from anywhere, unlike a lexical —
       DOES leak into observable state, which is a mutsu-internal two-pass-registration
       artifact with no Rakudo equivalent (Rakudo composes once, at compile time, full stop).
       The regression tests for cases 1/2 in the table above therefore isolate their class
       declarations inside their own `module { ... }` block (which gets its own independent
       `hoist_type_decl_shells` pass, and — as the FIRST statement of a fresh block — is never
       shelled at all, per that function's own doc comment) rather than sitting at
       `t/role-body-composition-timing.t`'s top level after 16 preceding assertions, where they
       otherwise WOULD be shelled and show a `$GLOBAL::`-count of 4, not 2.

`ClassDeclModifiers`/`RoleCompositionCx`/`ClassBodyCx` all gained an `is_hoisted_shell: bool`
field threaded from `exec_register_class_op`'s `__hoisted` custom-trait check down to the
class-composition guard.

## Raku quirks found along the way

Two Rakudo behaviors complicated writing a reliable case table and are worth recording:

- **`class X does R {}` composes at *compile* time**, before any mainline statement runs —
  confirmed by writing to a package variable from the role body and reading it back after a
  runtime `our $x = 0;` reset: the reset always wins, because it runs strictly after
  compile-time composition regardless of its textual position. The same applies to a *literal*
  `but`/`does` (`1 but R`), which gets constant-folded and composed at compile time the same
  way — even wrapped in a parameterless sub. A *variable*-valued `but`/`does` (`$n but R`) does
  compose at genuine runtime.
- **A bare, method-less role's deferred body writing to a lexical closure from inside a
  *runtime* `but`/`does` does not reliably reach the declaring frame** unless the role also
  declares a method. This reproduces even at the outermost mainline scope with real Rakudo
  2026.06. Every case-table script and the new regression tests therefore give each probe role
  a trivial method and read counts through `$GLOBAL::` package variables (accumulated without
  an intervening reset) rather than lexical closures, to avoid this and the compile-time-
  composition confound at once.

## Tests

`t/role-body-composition-timing.t` gained 7 new assertions (plan 16 → 23) encoding the case
table directly: two distinct classes, a same-class loop redeclaration, `but` across two base
types, and `does` on one/two-same-class/two-different-class instances — verified to pass
identically against `raku` 2026.06 and mutsu.

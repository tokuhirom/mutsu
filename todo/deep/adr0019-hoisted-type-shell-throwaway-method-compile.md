# A hoisted class/role forward-reference shell always pays a throwaway per-method compile that D3-8 exists to eliminate

While reverifying `todo/tickets/adr0019-method-body-compile-dedup-remnants.md` item 2's
"separate finding" footnote (`compile_role_methods`'s eager per-registration compile, flagged
2026-08-19 as "not root-caused to the same certainty" as the proto-method fix and needing "a
future session" to gdb a hit-count sweep), this session did that sweep and found a precise,
general root cause — bigger than the footnote assumed, and affecting **classes as much as
roles**, not just roles.

## Root cause

`add_class_decl_plan` (`src/compiler/decl_plan.rs:182-227`) and `add_role_decl_plan`
(`:511-542`) both compute `package_name: None` — and therefore leave every entry of
`method_compiled_keys` (from `compile_method_body_keys`) `None` — whenever the statement being
compiled is a `__hoisted` forward-reference shell (`is_hoisted_shell` from
`hoist_type_decl_shells`, `src/compiler/helpers_ast_utils.rs:630-692`). The comment at
`decl_plan.rs:200-210` explains the intent: "only the SOURCE-ORDER declaration's plan is ever
the one D3-8b/c would install from ... Skip the (otherwise-redundant) compile there."

That premise is only half right. The *plan* computed for the shell is indeed redundant — the
shell's own class/role identity is superseded when the real, source-position declaration
registers moments later — but skipping the **compile-key** does not skip the **runtime
registration-time throwaway compile**. `exec_register_class_op` / `exec_register_role_op` still
unconditionally call `compile_class_methods` / `compile_role_methods`
(`src/runtime/accessors_resolve.rs`) for the shell's own registration pass, same as any other
registration. Since the shell's `CompiledMethodDecl`s all have `compiled_routine_key: None`,
`class_body_method_decl` / `role_body_method_decl`'s `matched_compiled_fn` lookup
(`src/runtime/registration_class_body_method.rs:140-155`,
`src/runtime/registration_role_method.rs:218-233`) always misses, so every method on the shell
gets the full pre-D3-8 registration-time compile (`compile_method_def_in_place_with_dist`,
`src/vm/vm_stats::record_method_body_runtime_compile()`) — and then that compiled
`MethodDef`/`compiled_code` is thrown away wholesale when the real declaration's registration
runs moments later and replaces it with a *freshly built* `MethodDef` set (this one correctly
keyed, since the real declaration is not a shell — `matched_compiled_fn` succeeds for it, no
throwaway compile). So the throwaway compile the shell pays for is not merely redundant with
the real one — it is **100% wasted work**: its result is never read by anything before being
discarded.

## Confirmed reproduction

`MUTSU_VM_STATS=1 ./target/debug/mutsu <file>` reports
`method_body_runtime_compiles` (`src/vm/vm_stats.rs`). Two synthetic checks, run this session:

```raku
# no hoisting trigger: method_body_runtime_compiles=0
class Foo { method a { 1 }; method b { 2 }; method c { 3 } }
Foo.new.a;
```

```raku
say "hello";  # ANY runtime statement before the class triggers hoisting
class Foo { method a { 1 }; method b { 2 }; method c { 3 } }
Foo.new.a;
```
→ `method_body_runtime_compiles=3` (exactly the 3 methods, once each — not scaling with
`.new`/method-call count, confirming it is a one-time-per-shell-registration cost, not the
same *unbounded per-call* shape the proto-method bug had, which was already fixed separately —
see below).

`hoist_type_decl_shells` (`helpers_ast_utils.rs:630-651`) hoists **every** class/role
declaration that appears after the first non-declaration/non-pragma statement in its
containing block (`seen_runtime_stmt`), regardless of whether anything actually
forward-references it. In practice this means most real files trigger it: `t/*.t` files
routinely open with `use Test; plan N;` (a `Call` statement, not in the exclusion list), so
essentially every class/role declared anywhere in the file after that point is hoisted-shelled.
This is confirmed by the numbers already on record in
`todo/tickets/adr0019-method-body-compile-dedup-remnants.md`'s 2026-08-19 update:
`t/role-pun-build-tweak.t` (21, one throwaway compile per role-method declaration — ~20 roles
each with 1-2 methods, each declared and used exactly once) and `t/text-csv-battery.t` (150,
from the bundled Text::CSV module's own class/role bodies loading once at `use` time) — both
counts track 1:1 with *declaration* count, not *call* count, matching the shell-registration
root cause identified here rather than a per-call bug.

## Why this is `todo/deep/`, not a same-session ticket fix

- **Breadth**: this affects essentially all non-trivial OO Raku code compiled by mutsu (any
  class/role declared after an earlier runtime statement in its lexical scope), not a narrow
  corner — so a wrong fix has broad blast radius.
- **Needs a correctness check before optimizing**: the natural fix is to skip the method-body
  compile entirely during a shell's own registration (since its `compiled_code` is never read
  before being discarded), not to try to give the shell a working compile key (the class side
  cannot share a key with the real declaration anyway — `type_decl_shell_body` truncates the
  shell's class body to a declaration-only subset, a genuinely different body from the real
  one, per the comment at `helpers_ast_utils.rs:694`). Before skipping the compile, it must be
  verified that **no code path between the hoist point and the real declaration's source
  position ever calls a method on the shell-registered type** (forward-referenced types are
  used for signature/`.isa`-style type-checking prior to their real body running, as far as
  this session confirmed by reading, but this needs verification against roast/spec, not an
  assumption baked into a low-risk-looking skip).
- **Needs new plumbing**: `is_hoisted_shell` is known at compile time
  (`decl_plan.rs`) but is not currently carried into `CompiledClassDeclPlan` /
  `CompiledRoleDeclPlan` (`src/opcode.rs:2808`, `:3189`) for `exec_register_class_op` /
  `exec_register_role_op` to read at runtime and pass down to
  `compile_class_methods` / `compile_role_methods` as a skip flag.

## Relationship to the two items already tracked

- This is **not** the same bug the proto-method fix (`PR #6655`,
  `Registry::proto_compiled_cache`) addressed: that was an *unbounded, per-call* recompile
  (`run_proto_method` built a fresh uncompiled `MethodDef` on every dispatch). This is a
  *bounded, one-per-hoisted-declaration* wasted compile — annoying but not runaway.
- This generalizes and supersedes the "separate finding, needs its own investigation" footnote
  on `todo/tickets/adr0019-method-body-compile-dedup-remnants.md` item 2 (which attributed the
  symptom only to `compile_role_methods`/roles); it is now confirmed to be a `hoist_type_decl_shells`-wide
  issue affecting `compile_class_methods` identically. That ticket has been narrowed to drop the
  footnote and point here instead.

## Suggested direction (not attempted this session)

Thread a `skip_method_compile: bool` (or equivalent) from `is_hoisted_shell` through
`CompiledClassDeclPlan` / `CompiledRoleDeclPlan` into `exec_register_class_op` /
`exec_register_role_op`, and have `compile_class_methods` / `compile_role_methods` (or their
callers) skip the throwaway compile when the current registration is a shell pass — after the
forward-reference-usage verification above. This removes the wasted compile without needing
the shell to have a working `compiled_routine_key`.

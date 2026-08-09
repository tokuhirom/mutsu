# ADR-0019 D8-2: role deferred-body composition runs precompiled chunks

Every consumer of a role's deferred (non-declaration) body statements —
`run_role_body_for_composition` (pun, `does`, runtime mixin) and
`run_composed_role_deferred_body` (parametric role composition) — now runs
D8-1's precompiled `DeferredBodyOp`s instead of re-parsing/re-compiling the raw
`Stmt`s on every composition. `RoleDef::deferred_body_stmts` is now write-only,
kept only until D8-4 drops it for good.

Verification followed the D7/D8 design doc's V1/V2 case tables: V1 (a
parametric role whose body declares a nested class referencing a type
parameter, composed at two different type arguments) is covered by the
existing `t/generics-nominalizable-class.t`; V2 (once-per-composition
side-effect timing across runtime mixin / static `does` / role pun / two
classes composing one role / a parametric role composed twice) was checked
against both `raku` and mutsu's pre-D8-2 baseline — mutsu's existing
divergences from `raku` here are pre-existing and out of this slice's scope
(memoization is role-global, not `(role, target)`-keyed), so the gate was
"identical to the pre-D8-2 baseline," not "matches raku."

Two real bugs surfaced during that verification, both fixed rather than routed
around:

1. **A `Plain`-kind deferred statement's package is ambient at the composition
   call site, not the role's own package.** The initial D8-2 attempt compiled
   every non-`TokenRule` op's chunk against the role's own package (matching
   D8-1's original assumption). `t/generics-nominalizable-class.t`'s `my
   package G { class A is Array[T] {} }` inside a parametric role body caught
   this: only a `TypeDecl` op (a nested `class`/`role` directly in the role
   body) is actually package-independent — every consumer explicitly overrides
   `current_package` to the role's name for exactly that op kind. A `Plain`
   statement's true package is whatever was ambient when composition started
   (mainline vs. an enclosing `package Foo { ... does R[...] }`), which is a
   per-composition fact unknowable at role-declaration compile time. Fixed by
   only compiling a chunk for `TypeDecl`; `Plain` and `TokenRule` both keep
   `chunk: None` and fall back to running the raw statement
   (`run_compiled_block_raw`/`run_block_raw`, matching the pre-D8-2 writeback
   and topic semantics exactly — not `run_decl_expr`, which restores the
   topic per statement instead of once for the whole body).

2. **A role's `__hoisted` forward-reference shell is not a throwaway stub.**
   D8-1 skipped compiling `deferred_body_ops` for a `__hoisted`-marked role
   declaration, reasoning by analogy with `add_class_decl_plan`'s class-shell
   handling that the compile would be "fully redundant." For a role this
   reasoning doesn't hold: `rust-gdb` confirmed a top-level role's `__hoisted`
   declaration and its "real" source-position declaration are the *same*
   compiled plan (same `RegisterDecl` index, full body, all methods) — the
   role hoist shell "keeps the whole original body," unlike a class's. Gating
   `deferred_body_ops` on `is_hoisted_shell` therefore left it permanently
   empty for every top-level role with a deferred body statement, silently
   skipping composition side effects. `t/indirect-declarator-names.t`'s `role
   RIndirect { my constant rname = 'rsecond'; ... method ::(rname) {...} ...
   }` caught this: the indirect method name never resolved because the
   constant that names it never ran. Fixed by always computing
   `deferred_body_ops`, decoupled from the hoisted-shell package gating that
   (correctly) still skips `method_compiled_keys`.

A third, adjacent bug was caught by the same verification pass but traces back
further, to D7-4: `RoleBodyOp::Deferred`'s catch-all classification also
matches `SetLine` source-line markers and the `__mutsu_stub_die`/
`__mutsu_stub_warn` stub markers, but `walk_role_body`'s own runtime dispatch
never defers either (`Stmt::SetLine(_)` is a silent skip; a stub marker sets
`is_stub_role` instead). Once bug 2's fix made `deferred_body_ops` compile
unconditionally, a *method-only* role body (no real deferred statement) still
produced a non-empty `deferred_body_ops` from its `SetLine` markers alone —
diverging from `deferred_body_stmts.is_empty()`, which baseline's callers rely
on to skip composition-time deferred-body execution entirely.
`t/role-double-parametric-args-distinct.t`'s `role R5[&f] { method v() { f(3)
} }` caught this: composition spuriously entered
`run_composed_role_deferred_body` and called `bind_type_capture("&f", ...)` —
a call meant only for `::T`-style type captures — clobbering `&f`'s env
binding with a captured type object instead of the callable value, so `f(3)`
inside the method died with "Unknown function: f". Fixed by filtering
`SetLine` and the stub markers out of `compile_role_deferred_body`'s input,
keeping `deferred_body_ops.is_empty()` in agreement with
`deferred_body_stmts.is_empty()` for every role.

Verified via the full `t/` suite (28,037 tests, all passing — including all
three regression tests above), every whitelisted `S06-signature`/
`S12-*`/`S14-*` roast file with the release binary, and the bundled-library
test-suite gate (`scripts/battery-testsuite.sh`, 158/164 passing, unchanged
from before this slice — `OO::Monitors`, the heaviest bundled parametric-role
consumer, fully green).

D8-3 (the `run_role_submethod` rider) and D8-4 (dropping
`deferred_body_stmts` outright) remain.

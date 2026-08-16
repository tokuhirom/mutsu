# Composing the same parametric role twice with different type args now dispatches correctly

`class A does R[Int] does R[Str]` where `role R[::T] { multi method foo(T $t)
{...} }` correctly composed BOTH multi candidates into `A`, and multi
dispatch correctly selected the right candidate per argument type — but the
value the selected candidate's *body* saw for the bareword type `T` was
wrong:

```raku
my role R[::T] { multi method foo(T $t) { "T=" ~ T.^name } }
my class A does R[Int] does R[Str] { }
say A.new.foo(5);    # was: "T=Str" (wrong, should be "T=Int")
say A.new.foo("x");  # was: "T=Str" (correct, but only by accident)
```

Root cause: `Registry::class_role_param_bindings` is a flat `class -> (param
name -> value)` map, and each composition of the same role overwrote the
previous composition's `T` entry (`does R[Int] does R[Str]` left `T => Str`
for both candidates' bodies, regardless of which one dispatch actually
selected).

Fixed by stamping each composed candidate's role type-parameter bindings
directly onto its own `MethodDef` (`role_param_bindings: Option<Arc<Vec<(String,
Value)>>>`) at composition time (`compose_role_into_class`,
`registration_class_compose.rs`), and overlaying those bindings on top of
the class-level map at dispatch (`call_compiled_method`,
`vm_method_dispatch.rs`) and at candidate signature matching
(`method_args_match_for_invocant`, `resolution_method.rs`). The class-level
map is applied first in both places and kept as the base layer — it also
carries per-composition rename entries for a nested generic class declared
inside the role body (`role R[::T] { my package G { class A is Array[T] {}
} }` renames `G::A` to `R::G::A[Int]`/`R::G::A[Str]` per composition, stored
in the same map keyed by the class's unparametrized name) — only the
type-parameter keys are overridden per candidate, so both mechanisms that
share this map keep working together.

New test: `t/role-double-parametric-multi-dispatch.t` (12 assertions,
verified against real `raku`). Full `t/` suite (3185 files) clean; targeted
sweep of all 21 whitelisted `S14-roles/*` files plus 96 `S12-*` files on
release, all pass — including `t/generics-nominalizable-class.t`, whose
nested generic-class naming was caught regressing mid-implementation by this
exact overlay-ordering issue and fixed before landing.

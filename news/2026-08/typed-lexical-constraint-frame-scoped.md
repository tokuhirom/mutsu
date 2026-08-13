# Typed routine lexicals no longer leak their constraint across frames (Text::CSV fully green)

A typed scalar `my` inside a routine used to register its type constraint in
the global bare-name-keyed `var_type_constraints` map, which survives the
routine's return. Any same-named variable assigned later — in the caller, in
another routine, even in another compunit — was then checked against the dead
frame's constraint. Text::CSV `t/66_formula.t` aborted on exactly this: the
script's untyped `my $e;` was poisoned to `Str` by `method string`'s
`my Str $e = $!esc;`, so the script's `CATCH { default { $e = $_ } }` died
with "Type check failed in assignment to $e; expected Str but got Any"
(`todo/deep/bare-name-type-constraint-store-is-scope-blind.md`).

The fix aligns typed scalar `my`/`state` declarations lexically inside
routines with what typed *parameters* already did (`bind_param_type_constraint`):

- The compiler emits a new `SetVarTypeScoped` opcode for them (decided at
  compile time from `is_routine || lexically_in_routine`; `our`, dynamics,
  `@`/`%` containers, and mainline declarations keep the both-store
  `SetVarType`). It registers the constraint ONLY in the env-scoped
  `__mutsu_type::` metadata, which dies with the frame and travels with a
  captured closure env — so closures escaping the frame keep enforcement,
  and EVAL'd re-assignment inside the frame still sees it.
- The return merges no longer copy a callee's own `__mutsu_type::<local>`
  entry back into the caller env: `is_callee_local_sym` unwraps the metadata
  prefix, and `merge_method_env`'s skip predicate does the same. This also
  fixes the pre-existing *overwrite* leak, where `sub f(Int $x is copy)`
  replaced a caller's `my Str $x` env metadata with `Int` on return.
- Nil-reset for typed scalars moved from the read paths into the SetLocal
  store path (`typed_scalar_nil_seed_value`, shared with the declaration
  seeding): a Nil ASSIGNED to a typed scalar stores the nominal type object
  (native zero/empty for native types) instead of relying on the reader to
  consult the global map. The Nil-read type-object conversion sites keep the
  global-map-only fast probe on purpose — a `Mu $b = Nil` parameter default
  must read as Nil, not as `Mu`.

Pinned by `t/typed-lexical-constraint-frame-scoped.t` (12 tests: the method
and 0-arg-fast-path leak shapes, the overwrite shape, in-frame and escaped-
closure enforcement, EVAL, state, unassigned-read-as-type-object, and outer
constraint survival across a shadowing call).

With this, Text::CSV `t/66_formula.t` passes 83/83 and the functional suite
is fully green: 33/33 files minus only `t/99_meta.t` (needs ecosystem
`Test::META`, a dist-metadata QA test unrelated to CSV). The residual
scope-blindness (`@`/`%` containers, mainline bare blocks, the
untyped-shadow clear) stays tracked in the deep ticket.

# The read-modify-write ops stop re-interning their target's name

`++`, `--` and the fused compound assignment `$x OP= rhs` each take their
target's name from a constant-pool operand. Their tail then probes that name
against four separate `Symbol`-keyed stores — the readonly registry, the env,
the declared-type lane and the native-int constraint — and every one of those
probes took a `&str` and re-interned it. That is a thread-local `RefCell`
borrow plus a string hash per probe, four times per store, for a name whose
symbol the chunk has already memoized.

`CompiledCode::const_sym` has interned every constant-pool name once per chunk
since #7736. The three op entry points now read it once and hand it down the
whole tail, using the `_sym`/`_for` variants that already existed at the bottom
of each lane (`is_readonly_sym`, `get_env_with_main_alias_sym`,
`set_env_with_main_alias_sym`, `var_type_constraint_sym`). The missing links in
the middle — `name_is_readonly_binding`, `check_readonly_for_incdec`,
`check_incdec_type_constraint`, `maybe_wrap_native_int`,
`wrap_native_int_arithmetic_result`, `normalize_incdec_source_with_type`,
`store_scalar_by_name` and `store_named_scalar_rmw_result` — grew a variant
taking `Option<Symbol>`, so a caller without a baked symbol (the slotless
`$.attr++` accessor path, which has no constant-pool index) still works exactly
as before.

## Measured

Release, callgrind, `tmp/bench-ok-2k.raku` = `use Test; plan 2000; for ^2000 {
ok 1, "x" }` under `MUTSU_REAL_TEST=1`, warm precompilation cache, at
`83f3ecbc`.

| caller | interns/assertion before | after |
| --- | --- | --- |
| `types::name_is_readonly_binding` | 1.00 | 0 |
| `vm_var_assign_typed::check_incdec_type_constraint` | 1.00 | 0 |
| `vm_var_assign_typed::store_named_scalar_rmw_result` | 1.00 | 0 |
| `vm_var_assign_post_incdec::exec_atomic_compound_var_op` | 1.00 | 0 |

`Symbol::intern` calls 62,485 → 54,485 (−8,000, −12.8%); whole run 473.03 M →
471.68 M Ir (−0.29%). The per-caller counts come straight out of the raw
callgrind file rather than `callgrind_annotate --tree=caller`, which truncates
its caller list and re-attributes across inlined copies — the annotated tree
charged only 7,080 of the run's 62,485 interns to any named caller.

The cold-cache warning in [#7766](https://github.com/tokuhirom/mutsu/issues/7766)
is real and cost a run here: the first post-rebuild measurement read 769.84 M Ir
and 55,048 interns, both inflated by a cold precompilation cache. Discard it and
measure the second run.

## What this is a slice of

[#7766](https://github.com/tokuhirom/mutsu/issues/7766) unit 2. The issue's
remaining four `_sym` splits — `call_compiled_function_named_inner`'s
`fn_package`/`fn_name`, `user_method_overloads` + `has_user_method`,
`multi_arg_type_keys`' receiver class name, and the resolution layers
(`find_compiled_function_inner` / `resolve_function_multi_cached` /
`fn_keys_for_base` / `push_multi_dispatch_frame_with_winner`) — are untouched
and the issue stays open for them. Unlike this family, each of those is a `&str`
API fed by a *further* `&str` API, so the fix is a chain grown up the call graph
rather than a local one.

## Regression cover

`t/routines/signature/param-bind-symbol-keys.t` grew a section for this family.
What a symbol mix-up produces here is not a slowdown but the *wrong name's*
entry being read or written, so each case gives its neighbours distinguishable
values: an `int8` that wraps beside a boxed `Int` that does not, a subset
constraint re-checked after the mutation, a readonly parameter and a sigilless
bind that must both reject `++`, two same-named locals in sibling scopes, an
`our` scalar, a dynamic variable, and the topic's rw writeback. All 55
assertions were verified against Rakudo v2026.07 first.

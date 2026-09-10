# The binder reads its callee's baked parameter symbols, and the last scalar-declaration env key gets pre-interned

Two of the three units named in [#7766](https://github.com/tokuhirom/mutsu/issues/7766) — the
successor to the [#7736](https://github.com/tokuhirom/mutsu/issues/7736) /
[#7762](https://github.com/tokuhirom/mutsu/pull/7762) campaign that cut the per-`Test`-assertion
`Symbol::intern` budget from 72 to the mid-30s. Both were the same shape: a name that had *already*
been interned once, somewhere the hot path could reach, being re-hashed per call anyway.

## The binder's parameter names (#7766 unit 1)

`CompiledFunction::precompute_param_name_syms` interns every `param_defs[i].name` when a routine is
registered. `bind_function_args_values_inner` did not read it: it kept a per-parameter `OnceCell`
and interned the name on first use in each call. Once per parameter per call is better than once per
helper — that was #7762's improvement — but it is not zero, and binding is the single hottest thing
a `Test` assertion does.

`bind_function_args_values_with_syms` is the new entry point: the two existing ones
(`bind_function_args_values`, `..._with_argspec`) delegate to it with an empty slice, so none of the
~16 callers that build `param_defs` by hand — a `FunctionDef`, a `SubData`, a synthesized signature —
needed touching or lost anything. The one caller that *does* hold a registered routine,
`call_compiled_function_named_inner`, passes `&cf.param_name_syms`, and that single site covers every
binder call the benchmark makes.

The slice is index-parallel to `param_defs`, so the interesting failure mode is misalignment: a
parameter bound, marked readonly or cleared under a *neighbour's* name. Two things guard it. A slice
whose length does not match `param_defs` is treated as empty rather than trusted, so a vector left
over from a `param_defs` mutated after the precompute degrades to the old lazy intern instead of
mis-keying a binding. And each of the two loops that resolves a name `debug_assert_eq!`s the baked
symbol against a fresh intern of `pd.name`, which makes a genuine index mismatch loud in CI's debug
runs (`gc-stress` / `jit-stress`).

## `__mutsu_scalar_bind_no_container::` (#7766 unit 3)

`my $i := 42` binds straight to a value, so the name owns no `Scalar` container and
`$i.self =:= $i` is True where `my $a = 42` answers False. That fact is recorded under
`__mutsu_scalar_bind_no_container::<name>`, and **every** scalar `my` declaration cleared the key
speculatively so a redeclaration could not inherit an earlier same-named variable's state.

Each of its four sibling keys — `locals_alias_sym`, `locals_readonly_sym`,
`locals_deleted_index_sym`, `locals_bound_slice_sym` — already has a per-local pre-interned vector on
`CompiledCode`; this one was the entry missing from the set, so it paid a `format!` plus a
`Symbol::intern` per declaration. It now has `locals_scalar_no_container_sym`, filled in
`compute_locals_sym` like the rest, and the key shape moved next to theirs in `runtime/utils.rs`.

The speculative clear also got the gate the family convention implies: a monotonic
`scalar_bind_no_container_possible()` latch, armed by the one site that creates the key (through
`insert_sym_noting`, since a plain `insert_sym` deliberately does not run `note_env_key`). A program
that never makes such a binding now skips the clear entirely — which matters beyond the intern,
because it also skips the `env_mut()` that could CoW-deep-clone the frame env on every `my $x`.

## Measured

Release + callgrind, `use Test; plan 2000; for ^2000 { ok 1, "x" }` under `MUTSU_REAL_TEST=1`, warm
precompilation cache, second run after the rebuild (a cold cache inflates this benchmark by ~60% and
is not comparable to anything).

| | before | after |
| --- | --- | --- |
| whole run | 488.15 M Ir | 481.05 M Ir (−1.45%) |
| `Symbol::intern` calls | 120,822 | 97,088 (−23,734) |

Per assertion, from the `callgrind_annotate --tree=caller` caller counts (all three are linear in the
assertion count):

| caller | before | after |
| --- | --- | --- |
| `bind_function_args_values_inner` | 7.00 | 2.00 |
| `OnceCell::try_init` (the binder's per-parameter cell) | 5.00 | 0 |
| `exec_set_local_op_inner` | 2.01 | 0 |

−11.9 interns per assertion, about a third of #7766's measured 35.4 budget. The `OnceCell::try_init`
row is the binder's too: #7766's table attributed ~7 interns/assertion to the binder because
callgrind charges the `get_or_init` closure to `OnceCell`, so the binder's real cost was ~12 and the
unit was worth more than it looked.

`t/routines/signature/param-bind-symbol-keys.t` — the file #7766 names as the regression cover for
exactly this class of change, a *name* bound under the wrong key rather than a slowdown — grew 11
assertions: parameter positions stay aligned across a mixed
positional/array/named/slurpy signature and across a parameter that follows a destructuring
sub-signature, only the `is copy` parameter of a pair is marked writable, and the container-identity
marker is set, cleared by a same-named assigned redeclaration, and set again. All 32 pass under
rakudo too.

## Still open on #7766

Unit 2 — the dispatch names (`call_compiled_function_named_inner` interning `fn_package`/`fn_name`
from `&str` parameters, the resolution layers below it re-hashing the callee name, and the
receiver-class names in `multi_arg_type_keys` / `vm_call_method_compiled_cache`) — is untouched. It
is ~10 interns/assertion across five call sites whose `&str` APIs are fed by further `&str` APIs, so
doing it properly means growing a `_sym` variant chain up the call graph, which #7766 itself
anticipates may want its own split. The measurement is recorded on the issue.

A new residue came out of the re-measurement: 2 interns/assertion still charged to
`bind_function_args_values_inner` after the parameter names are free (an inlined by-name `Env`
operation, not the parameter path), and a 16-interns/assertion bucket callgrind attributes to a
`Vec::from_iter` — an inlining artefact whose real owner needs the per-string histogram #7766
describes. Neither is named in the issue's original table.

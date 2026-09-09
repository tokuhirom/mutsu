# Halve the `Symbol::intern` calls a Test assertion pays

[#7736](https://github.com/tokuhirom/mutsu/issues/7736) attributed the per-assertion
budget of the vendored `Test` module's `ok` and found `Symbol::intern` at the top:
**~78 interns per assertion, 5.6% of the run**, half of it reached from the signature
binder. The pattern was the one ADR-0037 and [#7571](https://github.com/tokuhirom/mutsu/issues/7571)
have been paying down elsewhere — a helper takes `&str`, so every call re-hashes a
name whose `Symbol` the caller already had, or which is a fixed literal that could be
interned once for the process.

Nothing here changes what is computed. Each site now carries the `Symbol` it already
had (or a pre-interned one) to the helper that needed it, instead of handing over the
string and making the helper find the symbol again.

## What was threaded where

**Fixed per-call keys.** `bind_function_args_values_inner` writes `@_` and probes
`self` / `?CLASS` on every compiled call, and `current_source_file` probes `?FILE` on
every caller-frame push. All four have pre-interned symbols in `symbol::wk` already;
they now use them.

**The declaring source file and package.** `CompiledFunction` has cached
`source_file_sym` and `package_sym` accessors, interned once per routine.
`enter_compilation_unit` was going through the `&str` form, re-hashing the whole
declaring path — 60+ bytes for a module — on every named call, and the package switch
re-hashed the package name the same way. Both now take the cached symbol
(`Interpreter::unit_of_source_sym`, `enter_package_guarded_with_sym`).

**The per-parameter path.** The binder resolved each parameter's name separately in
every helper it passed — the value bind, the type-constraint registration, the
readonly mark. It now interns the name at most once per parameter and hands that
symbol to `bind_param_value_sym` / `bind_param_type_constraint_sym` /
`mark_readonly_sym`. The intern is lazy (a `OnceCell` per parameter), so an arm that
never names the parameter still interns nothing.

**The store paths.** `SetLocal`'s slot symbol (`CompiledCode::locals_sym`) and
`SetGlobal`'s constant symbol now reach the env probe, the readonly check, the
typed-lexical clear, and the two `__mutsu_sigilless_*` metadata keys — the last of
which also drops two `format!` allocations per store, since those keys are memoized
per name symbol (`sigilless_readonly_key_for_sym` and its alias twin).

Two `&str` entry points that had no remaining callers after this are gone:
`Interpreter::type_meta_key_sym` folded into `type_meta_key_for_sym`, whose doc
comment now records that every caller holds a symbol.

`Env` gained two small helpers for the sites that needed them: `get_for`, for a caller
whose symbol is `Option` (a compiled slot has one, a hand-built chunk does not), and
`insert_sym_noting`, which latches the monotonic key-family flags `note_env_key`
maintains. That second one matters: `insert_sym` deliberately skips that latch,
because its existing callers pass ordinary lexical names, so a site converted *from*
a by-name `insert` — a placeholder parameter stored under its `^`-twigil name, a
`__mutsu_sigilless_*` metadata key — has to keep arming the flag or a later gated
fast path silently skips work it should do.

## Measured

Release build, callgrind, `use Test; plan 2000; for ^2000 { ok 1, "x" }` under
`MUTSU_REAL_TEST=1`, warm precompilation cache:

| | before | after |
| --- | --- | --- |
| `Symbol::intern` calls | 156,998 | 94,807 (−39.6%) |
| its inclusive cost | 21.57 M Ir (4.53%) | 13.18 M Ir (2.85%) |
| whole run | 475.87 M Ir | 462.67 M Ir (−2.8%) |

Per caller, the interns the change removed:

| caller | before | after |
| --- | --- | --- |
| `bind_function_args_values_inner` | 28,004 | 14,001 |
| `exec_set_local_op_inner` | 16,046 | 4,022 |
| `exec_one_dispatch` (`SetGlobal`) | 12,076 | 4,034 |
| `bind_param_value` | 10,001 | 0 |
| `bind_param_type_constraint` | 10,001 | 0 |
| `set_var_type_constraint_impl` | 4,044 | 22 |
| `check_readonly_for_modify` | 4,021 | 0 |
| `set_current_package` | 4,012 | 2 |
| `current_source_file` | 2,109 | 0 |
| `call_compiled_function_named` | 4,003 | 0 |

Counted a second way, with a temporary per-string histogram in `Symbol::intern` and
the steady-state slope taken between a 101- and a 201-assertion run, the per-assertion
budget fell from **72.0 interns to 35.4**. Instruction counts are deterministic and
load-independent, so these are the numbers the change was iterated against; a
wall-clock figure for a document must still come from the bench CI.

## Still open

The binder is down to about 7 interns per assertion but not to zero:
`CompiledFunction::precompute_param_name_syms` already bakes every parameter's symbol
at registration time, and threading that slice into
`bind_function_args_values_with_argspec` would remove the remaining per-parameter
intern outright. That is slice 2 of [#7736](https://github.com/tokuhirom/mutsu/issues/7736)
finished properly, and it needs a signature change across the binder's callers rather
than the local substitutions this change made.

The rest of what is left is spread thin across dispatch: the routine-name intern in
`call_compiled_function_named_inner`, the receiver-class names in the compiled-method
cache, and the `__mutsu_scalar_bind_no_container::` key, which wants its own
pre-interned per-local vector next to `locals_readonly_sym`.

`t/param-bind-symbol-keys.t` pins the behaviour the threading could get wrong — a
*name* being bound, marked or cleared under the wrong key rather than a slowdown:
`@_`, `self`/`::?CLASS`/`$?FILE`, positional/optional/named binding, readonly versus
`is copy`/`is rw`, placeholder and sigilless parameters, and typed-parameter
constraint scoping.

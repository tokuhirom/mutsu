# The compiled-function table shares its bodies

Twelfth perf slice for `todo/deep/vendor-real-test-module.md`. The on-demand
`callframe().code` object (`news/2026-09/callframe-code-object-is-built-on-demand.md`)
stopped building a `Sub` per named call, but the lazy frame it pushes instead
still captured what the `Sub` would be built from -- `cf.params.clone()` and
`cf.param_defs.clone()`, a `Vec<String>` and a `Vec<ParamDef>` deep-copied on
every entry and dropped on every return. It had to: `CompiledFns` owned its
`CompiledFunction`s by value, so a running call held only a borrow of its own
routine and nothing it could keep past the table's lifetime.

`CompiledFnMap` is now `FxHashMap<Symbol, Arc<CompiledFunction>>`. The table
API is unchanged for writers (`insert` wraps; `retain` hands out `&`), the
resolvers (`find_compiled_function`, the ADR-0066 direct-mapped call cache) hand
out `&Arc<CompiledFunction>`, and the named/light call paths take the `Arc` so
`LazyRoutineCode` holds one refcount bump of the routine instead of copies of
its signature. The cache slot still stores an address into the table -- now of
the `Arc` value slot -- under exactly the same `fns_id` validity argument.

Two other deep copies fell out along the way: an imported module's routines
were installed in both the importer's table and the per-import table by
cloning every body (`insert_shared` now shares them), and the `&?ROUTINE`
materialisation of a plan-compiled multi rebuilt its `Arc<CompiledFunction>`
from a clone (`Arc::clone` now). The registration side
(`register_compiled_sub_decl` / `register_proto_decl`) still receives
`&CompiledFunction` and clones into `FunctionDef::compiled`; that is a
declaration-time cost, not a per-call one, and is left as is.

## Measured

Callgrind, 300 `ok 1, "x"` in a loop under `MUTSU_REAL_TEST=1`, one-assertion
baseline subtracted, release build:

| | before | after |
| --- | --- | --- |
| per assertion | 244,707 Ir | 237,744 Ir |
| `<Vec<T,A> as Clone>::clone` | 5.0k | 1.4k |
| `<ParamDef as Clone>::clone` | 2.3k | 0 |
| `drop_in_place<ParamDef>` | 1.5k | 0 |
| `call_compiled_function_named_inner` (outer frame) | 224.9k | 218.0k |

**-2.8% per assertion**; -29.2% since the session opened at 335,929.

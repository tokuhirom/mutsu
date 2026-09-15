# The assertions that policed symbol threading were themselves interning

[#7766](https://github.com/tokuhirom/mutsu/issues/7766) unit 2 has been a
sequence of "thread the `Symbol` the caller already holds instead of
re-deriving it from the string" changes: #7871 did the binder's parameter
names, #7940 the builtin-type MRO catalog, #7979 the assignment path, #8346
the by-name call entry and the resolution probe below it. This round finishes
the resolution layers — and finds that the largest remaining source of
per-assertion interning was not the code being threaded, but the
**`debug_assert!`s written to police the threading**.

## What the profile said

Attribution was taken with a throwaway instrumented build: a counter in
`Symbol::intern` keyed by `(string, trimmed backtrace)`, dumped beside
`mutsu::dump_vm_stats()`, and read as the slope between a 100- and a
200-assertion run of `use Test; plan N; for ^N { ok 1, "x" }`. That is the
per-string breakdown #7766's body recommends, and it is what the caller tree
alone cannot give.

A debug build interned **49.06 times per assertion**. Where it went:

| call site | interns/assertion | what it is |
| --- | --- | --- |
| `baked_param_name_sym` | 20.00 | `debug_assert_eq!(sym, Symbol::intern(&pd.name))` |
| `bind_param_type_constraint_sym` | 5.00 | the same assertion |
| `source_file_sym_by_walk` | 4.04 | the same assertion, on `?FILE` |
| `set_current_package_with_sym` | 2.00 | the same assertion, on the package |
| `set_var_type_constraint_impl` | 2.00 | the same assertion |
| `multi_arg_type_keys` | 2.00 | receiver class name (unit 2 item 3) |
| `get_method_overloads` | 2.00 | MRO class names (unit 2 item 2) |
| `native_lever_a_user_override` | 2.00 | a `&str` wrapper over an existing `_sym` twin |
| `fn_keys_for_base`, `resolve_function_multi_cached`, `resolve_all_multi_candidates_cached`, `has_multi_candidates_cached`, `name_is_readonly_binding_for` | 1.00 each | the resolution layers |

**Thirty-one of the 49 were assertions.** Each one exists to catch a caller
that pairs a name with the wrong symbol, and each implemented that check by
interning the name again — which is precisely the operation the surrounding
code was changed to stop doing.

## Why that matters beyond a debug build

`cargo test` is a debug build everywhere in this repo, and the
`symbol::intern_calls()` budget tests
(`tests/named_call_intern_budget.rs`, `tests/closure_call_intern_budget.rs`,
`tests/regex_match_intern_budget.rs`) take their CI-enforced calibration from
it. So the budgets that are supposed to pin how much a call path interns were
dominated by assertion noise: a `Test` assertion's debug budget was 61, of
which roughly half was the assertions rather than the path under test.

## The fix

`Symbol::lookup` is the same check and does not intern. The argument is
#8346's: the passed-in symbol can only exist because someone interned the
name, so a present entry is the whole assertion. Six sites change that way.

`source_file_sym_by_walk` keeps its exact meaning by falling back to `intern`
on a lookup miss: a `?FILE` string that was never interned still yields a
fresh symbol, which then differs from `Env::source_file_sym` and fires the
assertion, as before. The common path interns nothing.

## The resolution layers, finished

The rest of unit 2 item 4. Three callers took a `&str` API whose `_sym` twin
already existed and whose symbol they already held —
`has_multi_candidates_cached`, and `native_lever_a_user_override` at two sites.
Four more layers gained `_sym` forms, completing the chain #8346 started at
`find_compiled_function_inner`: `fn_keys_for_base`,
`resolve_function_multi_cached`, `resolve_all_multi_candidates_cached` and
`push_multi_dispatch_frame[_with_winner]`.

One of them cannot always use the caller's symbol, and the reason is worth
recording. `fn_keys_for_base`'s index is keyed by the name's **base** —
`function_key_base_name` strips an arity suffix and any `::` prefix, returning
a suffix slice. The caller's symbol is therefore usable exactly when the base
*is* the whole name, the ordinary unqualified case; a qualified or
arity-suffixed name still interns its shorter base, as the `&str` entry point
does for everyone. Reusing the caller's symbol unconditionally would key the
index by the wrong string.

`MarkRwArgRefContext` additionally stops building a `String` per op: the callee
name is a string constant, so `CompiledCode::const_sym` hands the symbol over
directly and the `.to_string()` disappears with the intern.

## Measured

Same benchmark, same method, debug build:

| | before | after |
| --- | --- | --- |
| interns per assertion | 49.06 | **11.60** |

−76%. Every targeted row reaches zero. Read through the budget tests, a `Test`
assertion goes 58.0 → 10.0 interns, a `multi` call 36.0 → 15.0, a
`where`-constrained call 70.0 → 53.0, a pointy-block call 2.0 → 1.0 and a
bare-block call 1.0 → 0.0, all debug; the budgets are re-pinned to the new
numbers so the assertions cannot silently come back.

## Two notes for whoever measures this next

**Do not set `MUTSU_VM_STATS=1` while profiling interning.** The first run here
did, and `shadow_check_native_row_candidate` — an ADR-0019 shadow probe that is
a no-op unless that variable is set — showed up as the single largest row at
14.00 interns/assertion, ahead of everything real. Two more rows
(`time_before`, `time_after`) are the same artefact. The probe is gated
precisely so it costs nothing in a normal run, so measuring with it on measures
a configuration nobody runs.

**`--profile profiling` is the wrong build for this question on a small box.**
It was started first, for symbol-rich backtraces, and had not linked after ~25
minutes on a 4-core container. A plain debug build gives *better* attribution —
less inlining, so fewer of the `OnceCell::try_init` / `Vec::from_iter`
artefacts earlier rounds had to see through — and built in three minutes. The
debug build's absolute counts differ from release, but the question "which call
site" does not need release numbers.

## What is left

Unit 2 items 2 and 3 are untouched and #7766 stays open for them:
`user_method_overloads` / `has_user_method` want `_sym` forms (2.00 + 1.00 per
assertion, and the hot caller `resolve_method_with_owner_impl` also clones the
whole `Vec<MethodDef>`), and `multi_arg_type_keys` wants a
`value_type_sym(&Value) -> Symbol` beside `value_type_name` (2.00), which needs
the design call #8346 recorded — a pointer-keyed memo over the `&'static str`,
or a discriminant, but not a second 60-arm match.

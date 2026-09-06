# The full multi-candidate gather stopped walking the whole registry

`push_multi_dispatch_frame` builds the `callsame`/`nextsame`/`callwith`
candidate list on every dispatch that could establish a dispatcher — which is
*every* call that reaches `compile_and_call_function_def` or the OTF cache's
named branch, whether or not the name turns out to be a multi at all. It did so
through `resolve_all_multi_candidates`, which iterated **every key in the
registry's functions map, once per enclosing package**, testing a
`"<pkg>::<name>/"` string prefix.

Under the vendored upstream `Test` module that is one full registry walk per
assertion. Measured with callgrind on a 2 000-assertion `ok 1, "x"` loop
(release build), it was **8.53% of the whole run** — 112.7 M instructions for
2 004 calls, ~56 KIr each.

## The index already existed

`fn_keys_for_base` is the per-`fn_resolve_gen` index the resolver's own
candidate gathers run on: every registry key whose *base name* (the key minus
its package prefix and its `/<arity>[:types]` suffix, per
`function_key_base_name`) matches. Every key the prefix filter can match
reduces to the queried name's own base name under exactly that function, so the
index is a superset of the matches and filtering it gives an identical answer.
The gather simply never used it.

`resolve_all_multi_candidates_indexed` (`&mut self`, since building the index
mutates it) now filters the index instead. The `&self`
`resolve_all_multi_candidates` keeps the full walk for the one reflective caller
that cannot take `&mut self` (`resolve_code_var`); both spellings share one
body, and a unit test pins them against each other over the awkward key shapes —
a plain multi, a proto'd multi, an operator name carrying its own `/`
(`infix:<//////>`), a non-multi sub, and an unregistered name.

A side benefit: the index is captured once per generation, so the candidate
order handed to `sort_candidates_by_specificity` is now stable within a
generation rather than freshly hash-ordered on every call.

## Result

| 20 000 × `ok 1, "x"`, `MUTSU_REAL_TEST=1`, release | before | after |
| --- | --- | --- |
| wall clock (median of 10 interleaved runs) | 3.64 s | **3.36 s** |
| wall clock (min of 10) | 3.46 s | **3.19 s** |
| callgrind Ir, 2 000-assertion run | 1 323.6 M | **1 231.8 M** |
| `push_multi_dispatch_frame` share | 8.53% | **1.68%** |

`bench-class` / `bench-ctor` are unchanged, as expected — they were not paying a
registry-sized gather per call.

## Where it came from

`todo/perf/listop-call-bypasses-every-compiled-call-cache.md` asked for a
dispatch-outcome counter on `OpCode::ExecCallPairs` before optimizing anything.
That counter is now in place (`execcallpairs:compiled` / `:native` / `:carrier`
under `MUTSU_VM_STATS=1`) and confirmed the ticket's structural claim — 200 of
200 assertions take the carrier arm, under the real `Test` module and under the
native one alike. It also refuted the ticket's *cost* claim: profiling the same
loop put the entire carrier — `find_compiled_function`'s miss,
`try_native_function`'s miss, the env snapshot, the write log and the writeback
diff together — at 1.2% of the run. The registry walk above was the item worth
seven times as much. The ticket now records the real profile.

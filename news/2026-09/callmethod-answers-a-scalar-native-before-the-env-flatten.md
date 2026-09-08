# `CallMethod` answers a scalar native method before the env flatten

`exec_call_method_op_impl` collapses a scoped overlay env to a flat env before
any dispatch that gets past the pure-read accessor fast path. On a scoped env
that is a whole-scope map clone, so the cost of one method call is linear in how
many names are in scope anywhere up the stack. `CallMethodMut` has had a gate in
front of that guard since #7554 — `try_env_pure_mut_dispatch` answers a pure
native method on an immutable scalar receiver (`Str`, `Int`, `Num`, `Bool`)
without flattening, because such a dispatch provably reaches nothing that
captures or iterates the env. The plain `CallMethod` opcode never got the same
gate.

It wants it. `benchmarks/method-call.raku` — a `Point.distance-to` loop whose
body ends in `(...).sqrt` — flattened the env 10,000 times, every one of them
from `CallMethod`, and every one of them resolving to a native method. The
receiver is a `Num` inside a scoped method frame, which is exactly case 1 of the
existing gate.

Case 1 is now a shared helper, `try_env_pure_scalar_native_dispatch`, called by
both opcodes; `try_env_pure_mut_dispatch` keeps its `IO::Handle` output arm and
delegates the scalar arm. The name-exclusion list it consults grew `VAR`, which
`exec_call_method_op_impl` hard-codes into its `skip_native` seed, and is now
documented as shared by both paths rather than specific to the mut one — it only
ever makes the gate more conservative.

Everything the general path would have decided for such a receiver is still
decided identically: the same `quoted`/modifier exclusions, the same
junction-argument autothreading deferral, the same `native_lever_a_user_override`
gate, and the methods that own a dedicated branch on either path
(`new`, `WHO`, `protect`, `make`, `subst-mutate`, `hyper`/`race`, the
xxx-KEY/BIND-POS mutators, the `push` family, `VAR`) are left to it. Both
opcodes' remaining pre-probe branches are receiver-typed on values the arm never
accepts — `Junction`, `LazyList`, `Failure`, `Package`, `Instance`, `Array`,
`Regex`, `Proxy` — and the four that could otherwise have applied to a scalar
(`.return`, the NativeCall `is native` hook, `proto` body dispatch, and
`Exception.Str`-via-`message`) all sit *before* the flatten on both paths
already, so none of them is stolen.

Measured with callgrind (deterministic instruction counts, `MUTSU_JIT=off`):

| benchmark | before | after | |
| --- | --- | --- | --- |
| `method-call` | 1,116,758,033 | 875,193,995 | **-21.6%** |
| `bench-class` | 1,256,230,586 | 1,253,912,753 | -0.2% |
| `word-count` | 1,120,162,872 | 1,119,097,351 | -0.1% |
| `bench-fib` | 2,713,379,355 | 2,713,380,971 | 0.0% |

Wall clock on `method-call` (best of 7, release, `MUTSU_JIT=off`) goes
0.248 s -> 0.214 s. The change is inert wherever the gated shape does not occur,
which is what the `bench-fib` row is there to show.

`t/callmethod-env-pure-native-gate.t` pins both halves: the gated shape itself,
the two lexical views that must still be whole after a gated call (a closure
capture and a `MY::` read in the same frame), and the seven cases the gate must
leave to the general path. All sixteen agree with rakudo.

This does not close #7563. The guard still fires on every user-defined method
dispatch from a scoped frame, which is where the ticket's pathology now lives —
see the re-measurement recorded there.

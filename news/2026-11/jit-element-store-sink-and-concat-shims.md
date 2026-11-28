# An element store statement's sink, and a string interpolation, skip the JIT's generic step

Two ops of `for ^N { %h{"key-$_"} = $_ * 2 }` still left a JIT body through
the generic `step` shim (an `exec_one` call, its dispatch prologue and the
opcode-group match) for work that needs none of it:

- `%h{$k} = $v;` compiles to `IndexAssignLocal` followed by `SinkPopAssign`,
  the statement's sink, which only popped the value the store had just
  pushed. When the sink directly follows the store and no jump lands on it,
  the JIT now emits one `element_store_sunk` call. If the lean element store
  serves the statement, its result is the plain scalar it stored, whose sink
  is a no-op (no lazy list to reify, no `Seq` to touch, no `Failure` to
  raise), so the shim pops it itself; anything the lean lane declines runs
  both ops generically, as before.
- `StringConcat` (the interpolation) now has its own shim, `concat_step`,
  which calls `Interpreter::exec_string_concat_at` -- the one body the
  interpreter's arm runs too -- and reports a halt the way `step` does.

callgrind, profiling build, JIT on (on top of #19426):

| measurement | before | sink fused | + concat shim |
|---|---:|---:|---:|
| `benchmarks/hash-access.raku`, `BENCH_DET=1` | 54,151,036 Ir | 52,387,748 Ir | 51,304,218 Ir |
| store loop, per iteration | 3,346 Ir | 3,177 Ir | 3,065 Ir |

# A TRIR routine runs typed on the call that resolves it, too

ADR-0112 Step 3's second slice. It came out of asking why an empty `[]`
element of a JSON::Fast decode cost ~138,000 instructions when every routine
involved was TRIR-accepted.

## The first call of a routine ran untyped

A TRIR chunk was entered from untyped code in one place: `exec_call_func_op`'s
resolution-cache hit. A call site's first call misses that cache. It then
takes `dispatch_func_call_inner`, which resolves the name, fills the caches,
and runs the untyped body. That is one untyped run per call site, which sounds
harmless, and it is not:

- A routine called once never ran typed at all. A `sub spin(int $n)` looping
  1,000 times, called once from the mainline, executed 18,029 untyped ops
  against 10 now.
- A recursive-descent parser's outermost call is its first call. The first
  `parse-array` of a decode is the top-level array, so its loop over every
  element ran untyped. Each element's `parse-thing` call then went through the
  whole generic call protocol, about 55 untyped ops an element, with
  `gen-links=0` for the whole first `from-json`.

Two new doors now run the chunk on the resolving call:

- `dispatch_func_call_inner`'s compiled-function branch;
- `compile_and_call_function_def_at`, which serves a module's subs after the
  name cache is filled.

Both hand over the arguments exactly as a cache hit would see them: taken
straight off the stack, `VarRef`-tagged and unspread. So an `is rw` native
binds by its caller slot on the first call too. They apply the hit path's own
admissions: no named argument, no `|` slip, no junction, no callsite-line
marker, no aggregate shared into a `$` parameter, no multi, no wrapped
routine.

## `nqp::create` no longer goes through method dispatch

`nqp::create(IterationBuffer)` reached its allocation through
`call_method_with_values(ty, "CREATE")`. The resolution walk that dispatch
does before it reaches its own `CREATE` arm cost ~20K instructions a call.
`nqp::create` is the REPR-level allocation and never runs a user `CREATE`
method; rakudo prints nothing for a class whose `method CREATE` says
something. It now calls `dispatch_create` directly. A `create` in a TRIR loop
went from 5.1 µs to 1.8 µs.

## Measured

An empty `[]` element, from-json of 4,001 of them: ~138K → ~65K instructions
(callgrind, `--profile profiling`). The SPDX bench barely moves (~0.16 s),
because its outermost array holds only 727 records. What is left for `[]` and
`{}` is mostly the untyped bareword resolution of `List` / `Array` /
`IterationBuffer` / `Map` / `Hash`, and of `Uni` / `NFD` per slow string. That
is 10% of the whole SPDX decode, and it is the next slice (#9122).

`MUTSU_TRIR_DUMP=ops` now also lists each accepted chunk's ops and constants,
which is how the bareword sites were found.

Pins: `t/vm/codegen/adr0112-trir-first-call.t` (with
`t/fixtures/trir-first-call.raku`) and `t/vm/nqp-create-skips-user-create.t`,
both checked against rakudo.

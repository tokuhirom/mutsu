# Array::Sorted::Util's `inserts` loop: 2.64M -> 1.51M instructions per insert

Concurrent::PriorityQueue's `t/02-concurrent.rakutest` drives 32000
`inserts @keys, $k, @values, $v, :&cmp, :force` calls, each of which runs a
binary search plus a backward and a forward scan of equal keys, all written
as `nqp::while` loops over `cmp($needle, haystack.AT-POS($i))`. Several
general costs on that loop body went away (callgrind slope of the real
module, profiling build: **2.64M -> 1.51M Ir per insert**):

- A scalar declared in expression position (`(my int $j = $i)` inside an
  `nqp::stmts`, `(my $x = ...)` as a statement) now takes a local slot like
  the statement form. It used to live in `env` only, so every read in the
  loop was a by-name `GetGlobal` and every store a by-name `AssignExpr`.
  Declarations with a trait and compiler temporaries keep the env-only form.
- An assignment used as an expression (`($i = nqp::sub_i($i, 1))`) takes the
  statement form's scalar store lane instead of the full expression-assign
  cascade, which also wrote the env mirror on every store.
- `cmp(...)` where `&cmp` is a parameter or `my &cmp` of the running frame
  reads that slot (resolved by the compiler, as `&cmp` as a value already
  was) instead of resolving the name through the env on every call. This
  also stops a caller's same-named `&cmp` from answering first.
- `.AT-POS($i)` on a plain Array or List answers through the subscript's own
  fast path; `cmp` of two Ints skips the blob/range/stringifier probes; the
  `Order` values it builds no longer intern three strings each; and
  `nqp::eqaddr` of two enum values compares them without building the
  `eqv` cycle context.
- A sigilless parameter passed through nested multi candidates could leave
  its writeback source parked in the retain-on-miss list for the rest of the
  caller's body (the source was listed twice; the second copy no longer found
  the baked slot the first consumed). While parked, every method-table site
  lane declined, so each `haystack.AT-POS($i)` ran the full method-call
  cascade (~12k Ir instead of ~2k). The drain now skips a source it already
  applied.

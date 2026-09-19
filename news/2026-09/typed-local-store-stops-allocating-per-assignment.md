# The typed-local store stops allocating and hashing on every assignment

`my int $i = nqp::add_i($i, 1)` paid, on **every iteration**, one `malloc`,
one `free` and a thread-local hash probe — all to re-derive two things that
cannot change.

Every scalar store asks for the variable's declared constraint before it type
checks, coerces and native-wraps the value. That ask did two wasteful things:

1. **It copied the constraint out of the env.** `var_type_constraint_sym`
   ended in `tc.as_str().to_owned()`, handing back an owned `String` — even
   though every consumer in the store path (`is_definite_constraint`,
   `type_matches_value`, `try_coerce_value_for_constraint`,
   `wrap_native_int_by_constraint`) only ever reads it as a `&str`, and the
   copy is dropped before the store completes. Three bytes of `"int"`, one
   allocation and one free, per assignment.

2. **It rebuilt a constant through a thread-local.** The `__mutsu_type::<name>`
   env key the probe looks up is a pure function of the name symbol, fixed for
   the life of the process. `MetaNs::key` already memoized it — but in a
   thread-local `HashMap`, so each store still paid a TLS access and a hash to
   learn a constant it had learned on the previous iteration.

Both are now gone, unconditionally — no constraint is special-cased and no
path is skipped:

- `var_type_constraint_value_sym` answers with the env's own `Str` value,
  whose clone is a refcount bump, and `exec_set_local_op_inner` borrows the
  `&str` through it for the whole typed-store block. The owned-`String`
  wrapper stays for the callers that keep the constraint past that borrow, and
  the attribute fallback (`$!x = v`, whose type comes from the class registry
  rather than the lexical lane) still builds its own string because it has no
  stored value to borrow from.
- A one-entry memo on the `Interpreter` answers the meta-key question with an
  integer compare. One entry is enough precisely because the pathological case
  — a hot loop — stores to the same variable every time.

## Measurements

Under callgrind, deterministic and load-independent (100,000 iterations of
`while nqp::islt_i($i, N) { $i = nqp::add_i($i, 1) }`, startup subtracted):

| | instr/iter |
| --- | ---: |
| before | 4,784 |
| after | **3,806** (-20.4%) |

Wall clock over 10M iterations, timed in-script so startup is excluded:
507.6 -> **392.2 ns/iter** (-22.7%).

The saving is larger than the removed allocations and probes account for on
their own (~330 instr/iter). Dropping the owned `String` also let the compiler
inline `try_resolved_type_capture_name` into `type_matches_value` and fold its
byte scans, so `resolved_type_capture_name`'s per-check string searching and
`resolve_constraint_alias`'s `Symbol::lookup` — another ~460 instr/iter
between them — disappear from the profile as well.

This is not specific to `nqp::`: it is every typed lexical store in every
program, including the `my int` counters the vendored `Test.rakumod` keeps.

## Where that leaves the rakudo gap

The same loop against rakudo, 10M iterations, startup excluded:

| | ns/iter | vs. raku |
| --- | ---: | ---: |
| raku | 5.2 | 1x |
| mutsu, before `OpCode::NqpOp` | 1064.4 | 205x |
| mutsu, after `OpCode::NqpOp` | 489.7 | 94x |
| mutsu, after this change | **392.2** | **75x** |

Still two orders of magnitude, and the remaining distance is not made of
things like this one. mutsu's JIT emits a Cranelift *call* per opcode —
`vm_jit_support::noarg_shim` maps `OpCode::Add => helpers::add`, and `iadd` is
never emitted at all — while rakudo turns `nqp::add_i` into one MoarVM
instruction on a native register. Closing that means unboxed values across
opcode boundaries, which is
[#8831](https://github.com/tokuhirom/mutsu/issues/8831) and needs an ADR.

Pinned by `t/types/coercion/typed-scalar-store-constraint-paths.t`, which
covers each branch of the store block the borrowed constraint now flows
through: the type check, native wrapping, coercion types, the `:D` and Nil
resets, subset predicates, the attribute fallback, and an inner declaration
shadowing an outer one with a different type.

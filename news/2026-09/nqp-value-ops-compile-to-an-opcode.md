# `nqp::` value ops compile to an opcode instead of a string-named call

mutsu already compiled the `nqp::` **control-flow** forms (`nqp::if`,
`nqp::while`, `nqp::stmts`, `nqp::ifnull`, `nqp::handle`) to real jumps. Every
`nqp::` **value** op — `nqp::add_i`, `nqp::ordat`, `nqp::atpos_i`,
`nqp::iseq_i`, all 171 of them — was left an ordinary `OpCode::CallFunc`.

So each execution re-established, from the callee STRING, what is a fixed
property of the call site. `exec_nqp_call_op` drained the operands into a
`Vec`, spread `|EXPR` positions, unwrapped `VarRef`s into a second `Vec`,
scanned for a callsite-line marker, auto-FETCHed `Proxy` arguments into a
third, stripped `nqp::` off the name, and then walked up to six chained
`match op { ... }` tables — `call_nqp_interpreter_op` → `call_nqp_op` →
`call_nqp_op_process` → `call_nqp_op_text` → `call_nqp_op_str` →
`call_nqp_op_list` — until one claimed the name. `nqp::ordat` lives in the
fifth of those, so it paid four failed table walks, over 25, 72, 20 and 24
names, before reaching its own implementation.

Almost none of that work could change the answer. `nqp::` is a reserved
namespace of compiler-known primitives: no user routine can be declared
there, the ops bind no parameters, and the layer has no notion of a Raku
container — `call_nqp_op` opens by decontainerizing everything it was handed.
The `VarRef` wrappers the call path built for each operand existed solely to
let a callee's `is rw` parameter bind a container, and the nqp path unwrapped
every one of them again.

## What this inverted

The cost landed exactly where it hurts most. In rakudo, `nqp::ordat` is a
single MoarVM instruction and is ~15x faster than the equivalent
`.substr(3, 1).ord`; writing a module in `nqp::` is how the ecosystem's "fast"
end — `JSON::Fast`, `CBOR::Simple`, much of what they depend on — buys its
speed. Under mutsu that technique was a pessimization: an `nqp::add_i` loop
ran **2.0x slower** than the plain `$i = $i + 1` loop it was supposed to beat.

## What it does now

Resolve the op at compile time, the way NQP's own QAST compiler does, and give
it an opcode:

* `runtime/nqp_op_ids.rs` registers every supported op name with a dense `u16`
  id and the dispatch table that owns it;
* `try_compile_nqp_value_op` looks the name up once, at compile time, and emits
  `OpCode::NqpOp { id, arity }`, compiling the operands as ordinary
  expressions rather than through `compile_call_arg`. The no-paren 0-arg term
  form (`my $t = nqp::time;`, which rakudo's own `Test.rakumod` uses
  throughout) resolves to the same opcode instead of re-walking the chain from
  the bareword path;
* `exec_nqp_op` reads the operands into a buffer reused across ops, FETCHes a
  `Proxy` operand in place, and dispatches by id straight into the owning
  table — one `match op`, never six.

The registry is an optimization, never a semantic gate, and every way it can
be wrong degrades to the old path rather than to a wrong answer:

* a name it does not know — including every `nqp::` name that is not an op —
  keeps the `CallFunc` path, which reaches the same dispatch chain and raises
  the same loud `Unsupported nqp:: op` error. That guard is load-bearing:
  `nqp::index` answers -1 where Raku's `index` answers `Nil`, and nqp code
  branches on exactly that;
* a `|EXPR` spread or a named argument keeps `CallFunc` too, because then the
  operand count is not a compile-time fact and only the general path can
  spread it;
* an entry tagged with the wrong table falls back to the full chain when its
  table declines it.

So adding an op arm without registering it costs speed, not correctness.
`OpCode::NqpOp` also joins the JIT's `step_supported` set, since `CallFunc` —
what these ops were before they had an opcode — already was, and an nqp-heavy
loop is the kind of chunk the JIT exists for.

## Measurements

Instruction counts under callgrind, which are deterministic and
load-independent (startup, 8.9M, subtracted; 100,000 iterations each):

| loop body | before | after | |
| --- | ---: | ---: | ---: |
| `nqp::islt_i` + `nqp::add_i` | 8,330 instr/iter | 4,774 | **-42.7%** |
| `nqp::islt_i` + `nqp::ordat` + `nqp::add_i` | 14,066 instr/iter | 8,609 | **-38.8%** |
| `benchmarks/bench-json-fast.raku`, whole script | 3.95G instr | 3.40G | **-14.0%** |

The ratios that matter are the ones measured *within* one binary, since they
are immune to the code-layout noise that adding an opcode variant introduces
(a plain non-nqp loop moved 1.19x one way and another moved 0.91x the other
between these two builds, which is layout, not this change):

| | before | after |
| --- | ---: | ---: |
| `nqp::add_i` loop vs. the plain `$i = $i + 1` loop | 2.02x slower | **1.10x slower** |
| `nqp::ordat` loop vs. the `.substr(3, 1).ord` loop | 1.54x faster | **2.89x faster** |

Writing the inner loop in `nqp::` is now the faster choice under mutsu, as it
is under rakudo. The wall-clock trend belongs to the bench CI series rather
than to these local runs.

**Against rakudo, though, the gap is still two orders of magnitude**, and the
sentence this paragraph used to end with — that an nqp op now costs about what
the VM's own native `+` opcode does — measured mutsu against itself and read
as more than it was. Timed in-script over 10M iterations with startup
excluded, the `nqp::islt_i` + `nqp::add_i` loop is:

| | ns/iter | vs. raku |
| --- | ---: | ---: |
| raku | 5.2 | 1x |
| mutsu, before this change | 1064.4 | 205x |
| mutsu, after | 489.7 | **94x** |

Halving it is real and it removes the polarity inversion, but it does not
change the order of magnitude, and the native `+` opcode it now ties is just
as far from rakudo: mutsu's JIT emits a Cranelift *call* per opcode
(`vm_jit_support::noarg_shim` maps `OpCode::Add => helpers::add`), never
`iadd`, and operands stay NaN-boxed on a heap stack throughout. Closing that
is [#8831](https://github.com/tokuhirom/mutsu/issues/8831).

## What this does not fix

Profiling the `nqp::add_i` loop after the change puts nqp dispatch at ~13% of
it; the remaining string `match op` inside the one surviving table is ~2%, so
converting all 171 arms to id constants is not worth its churn. What dominates
now is general VM work that has nothing to do with `nqp::` — `SetLocal`
(15.8%), the `my int` type check on each assignment (4.7%), env lookups, and
two allocations plus two thread-local probes per iteration. The allocations
and the thread-local probes were the bounded part and are gone (see
`news/2026-09/typed-local-store-stops-allocating-per-assignment.md`); the rest
is the register/unboxed-IR half of
[#8673](https://github.com/tokuhirom/mutsu/issues/8673), tracked as
[#8831](https://github.com/tokuhirom/mutsu/issues/8831), which needs an ADR.

Pinned by `t/vm/codegen/nqp-value-ops-compile-to-opcode.t`, alongside the
existing `t/routines/dispatch/nqp-dispatch-fast-path.t`.

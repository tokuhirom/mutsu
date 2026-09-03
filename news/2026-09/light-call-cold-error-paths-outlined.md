# The hot call path carried its error messages in its stack frame

`call_compiled_function_positional_light_at` is the hottest dispatch path in
the VM (36.7% of `bench-fib`'s self time). Its stack frame was **1064 bytes**,
and a recursive benchmark touches that whole frame once per call.

A large part of it was dead weight. Four error paths were written inline in the
function body — two arity mismatches, a parameter type-check failure, and a
return-type-check failure — each with a `format!` and, for three of them, an
`X::TypeCheck::Argument` attribute map. None of that runs on a successful call,
but LLVM still reserves the stack slots for the `format_args!` argument arrays,
the `String`s, and the `HashMap`s in the one frame every call pays for. The
`perf annotate` symptom was a run of `movaps %xmm0,N(%rsp)` spill stores
carrying double-digit percentages of the function's self time.

All four are now `#[cold] #[inline(never)]` free functions taking exactly what
they need. The frame is **984 bytes**, and the behaviour is byte-identical —
the message construction moved verbatim.

## Measurement

Interleaved A/B of two release builds, median over 11 alternating runs on a
pinned P-core:

| benchmark | cycles | instructions |
| --- | ---: | ---: |
| `fib` | −7.3% | −1.7% |
| `bench-fib` | −4.5% | −1.7% |
| `bench-tak` | −0.7% | −1.2% |

The cold code this change moves is **never executed** by these benchmarks,
which is normally the signature of a binary-layout artefact rather than a real
win (see the discharge rule in
`todo/perf/late-august-call-path-slowdown-remainder.md`). Retired
**instructions** settle it: they are layout-insensitive, and they drop by
1.2–1.7%, so real work — the prologue/epilogue spill traffic for those slots —
is genuinely gone. The larger cycle delta on top of that is the frame and
I-cache behaviour the smaller frame buys.

984 bytes is still a large frame; whatever else fills it has not been
identified. `perf annotate` on the function is the way in.

# Closed-int-arithmetic subs run as native kernels

A sub whose signature is one to six plain scalar positionals (untyped or `Int`)
and whose body is closed integer arithmetic now runs as a native kernel
(ADR-19288 stage 1, #19290). Once it is hot, the body is lowered by Cranelift to
one machine function over `i64` values: comparisons are `icmp`, a self-call is a
direct machine call, and there is no frame, no binding and no boxing per call.
`fib`, `tak` and the `step` of `benchmarks/debug-guard.raku` are such subs.

The kernel is entered from the light call path's cache hit, under guards
checked once per entry: every argument a plain `Int`, no user infix operator in
the process, no wrapped routine, and every self-call in the body resolving to
this routine. When the kernel cannot continue (an overflowing `+`, `-` or `*`,
a zero divisor, `MIN div -1`, the depth bound) it bails, and the whole call is
rerun on the generic path, which promotes to a big integer, throws the
exception or reports the recursion. A kernel reads only its arguments and
writes only its registers, so the rerun cannot be observed. A routine that bails
on eight consecutive entries stops trying.

Callgrind, whole process, release build, kernels on against `MUTSU_KERNEL=off`:

| benchmark | off | on | rakupp v5.9.0 |
|---|---:|---:|---:|
| fib | 795.4M | 33.7M | 41.3M |
| bench-tak | 1,621.5M | 59.8M | 77.3M |
| bench-fib | 2,159.8M | 50.6M | |
| debug-guard | 958.2M | 542.4M | |

`MUTSU_KERNEL=off` disables kernels alone; `MUTSU_JIT=off` disables them with
the rest of the JIT. `t/vm/codegen/jit-int-kernel.t` runs its fixture all three
ways and requires identical output.

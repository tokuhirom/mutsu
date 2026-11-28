# Arithmetic loops run as one native loop

A counted `for` over an int range and a `while` whose body and condition are
closed `Int`/`Num` arithmetic now run as a loop kernel (ADR-19288 stage 2,
#19292). At an iteration boundary the loop driver hands the rest of the loop to
one Cranelift function: the slots the loop carries are loaded into machine
registers as `i64` or `f64` (each must hold a plain `Int` or `Num`), the body's
`my` variables live in registers, `$_` or the loop parameter is the counter, and
the loop runs with no interpreter dispatch, no per-iteration JIT entry, no
by-name topic lookup and no `SetVarDynamic`. At the end the slots are written
back through the ordinary store and the body's declarations are replayed with
their final values.

If an iteration cannot finish natively (an overflowing `+`, `-` or `*`, a zero
divisor, `MIN div -1`), the slots are written back as they were when that
iteration began and the driver reruns it generically, so a loop that outgrows
`i64` continues with big integers and a division by zero throws from the right
iteration with the right partial sums. A typed variable, a `Bool`, a `Str` or a
slot whose kind would change keeps the generic loop. A `while` is tried at entry
once the site is warm, and again after 256 iterations of a long loop.

Release build, kernels on against `MUTSU_KERNEL=off`, against rakupp v5.9.0:

| benchmark | Ir off | Ir on | wall off | wall on | rakupp |
|---|---:|---:|---:|---:|---:|
| int-arith | 197.5M | 8.3M | 16.8 ms | 6.2 ms | 6.4 ms |
| num-arith | 271.3M | 11.2M | 21.7 ms | 8.1 ms | 8.1 ms |
| time-parts | 975.9M | 26.7M | 75.2 ms | 8.8 ms | 9.7 ms |
| bench-mandelbrot | 494.8M | 68.2M | 39.1 ms | 12.6 ms | 6.5 ms |

(Wall clock is the minimum of 7 runs on the 4-core container.) A kernel entry
allocates nothing. `bench-mandelbrot` still enters its `while` kernel once per
pixel from an interpreted `for` whose body calls `.Num`.

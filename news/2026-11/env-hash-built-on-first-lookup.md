# `%*ENV` is built on its first lookup

`Interpreter::new` used to build `%*ENV` before running anything. That meant reading every
environment variable and passing each value through `val()`, so that `42` becomes an `IntStr`
as in rakudo. In a container with 187 variables this was 0.94M of `say "hello"`'s 6.3M
instructions, mostly the numeric parse of each value and the allomorph mixins, and it was paid
by every program, including those that never name `%*ENV`.

The interpreter's base tier (ADR-0086) is now a `DynBase`: the immutable map of built-in
dynamics plus *lazy entries*, each built on the first lookup that reaches it.

- `%*ENV` is the first lazy entry. Its builder comes from the runtime, so the env layer does not
  name it.
- Hit lookups are unchanged. A miss in the base pays one extra emptiness test.
- Key listings answer without building. Walks that need values (flatten, the closure-capture
  merge, the `DYNAMIC::` listing) build the entry.
- The GC visits only entries that have been built.

Start-up still asks for a few repository variables (`default_repo_dir`). It used to build the
whole hash to get them. While `%*ENV` is unbuilt, nothing has read it and so nothing has written
it, so these lookups now read the process environment directly.

The behaviour of `%*ENV` is unchanged. Values are still `val()`-ed, writes stick, and every
frame, thread and child process sees the same hash. `t/vm/env-hash-built-on-first-lookup.t`
pins all of this against rakudo.

Measured on `benchmarks/bench-startup.raku` (`MUTSU_JIT=off`, CI-style release build, 187
environment variables), with 2×40 interleaved runs for faults and wall time:

| | before | after |
|---|---:|---:|
| instructions (callgrind) | 6.30M | 5.43M (-14%) |
| minor faults | ~1,044 | ~1,029 |
| wall time | | -1 to -4%, within this box's noise |

CI runners have fewer environment variables, so the gain there will be smaller. (#19224)

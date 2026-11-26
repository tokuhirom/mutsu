# The `X::` exception classes are no longer built at start-up

Every mutsu process used to register about 370 built-in `X::` exception classes before running a
line. Each one cost a `ClassDef` and the interned names of its MRO, about a quarter of the
instructions of `say "hello"`. A program raises or names a few of them, if any.

They are now rows of a static class catalogue (ADR-19206, the first slice of #19206):

- The rows are one `&'static str` in `.rodata`, sorted by name and indexed by `const`-computed
  offset tables. The catalogue adds no load-time relocations.
- `Registry::classes` is now a `ClassTable`: the copy-on-write sharded overlay, with the
  catalogue beneath it.
- A catalogue class is built on its first lookup into a slot that every interpreter and thread
  in the process shares.
- A write copies the class into the overlay first. `remove` hides the row. An EVAL rollback
  restores the hidden rows along with everything else.
- A unit test checks that every row builds exactly the class the old `register_x` built.

Measured on `benchmarks/bench-startup.raku` with `MUTSU_JIT=off`, on a CI-style release build:

| | `Interpreter::new` | `build_builtin_registry` | whole run |
|---|---:|---:|---:|
| instructions before | 5.63M | 3.56M | 7.76M |
| instructions after | 4.19M | 2.09M | 6.31M |

That is 19% fewer instructions for the whole run. Minor faults fell by about 70. Against `main`
before #19204, faults went from ~1,220 to ~1,035 and median wall time fell 8-11% (2×40
interleaved runs).

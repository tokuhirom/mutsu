# A lean `%h{$k}++` for a plain declared `%` local

The word-frequency idiom `%counts{$w}++` reached its plain-hash lane only
after the package-body lexical seed and a set of by-name guards. Those guards
were a unit-lexical cell probe, a lexsub-alias probe (one string-hash lookup
per enclosing pointy loop), an `our` mirror probe, three env reads of the
binding, and a key `String` built for every step. That came to about 3,000
instructions per increment.

The four `*IncrementIndex` / `*DecrementIndex` opcodes already carry the base
container's compile-time slot. When the running chunk declares that slot as a
plain user `%` lexical, the step now takes the lean element store's checks
(#19306) and settles each one where it is cheap:

- the chunk's slot class;
- the process latches;
- the variable's `:=` pair, readonly flag, default and constraint;
- the hash node's own metadata.

It then steps a plain small `Int` element, or an absent one (which counts from
0), in place, with one hash probe and with the key read where it sits on the
stack. Anything else declines with the stack untouched and runs the
generic op as before: a bound or Proxy element, a `Bool`, `Str` or `Num`
element, or an overflow to a big `Int`.

callgrind, profiling build, JIT on, `BENCH_DET=1`:

| file | before | after |
|---|---:|---:|
| `benchmarks/word-count.raku` | 755,928,530 Ir, 342,602 allocations | 526,182,471 Ir, 242,635 allocations (-30%) |
| 100,000 × `%c{$_ % 10}++` in a sub | 450,677,620 Ir, 103,225 allocations | 275,880,156 Ir, 3,235 allocations |

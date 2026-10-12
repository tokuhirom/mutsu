# `Rat * Int`, core `Int` arithmetic and a `Rat` parameter check stop taking the long way

Four per-call costs in `bench-multi-dispatch` (#10111). The first three are in
the multi-method section, which computes `($r * 4).Int` in a
`multi method scale(Rat $r)` candidate; the fourth is in the operator section:

- **`Rat * Rat` and `Rat * Int` skip the generic multiply.** The opcode went
  through the junction wrapper and the numeric bridge, and the shared
  `arith_mul` cloned both operands for its Range and Mixin probes before it
  reached `rat_mul_checked`. A pair of plain `Rat`/`Int` operands now goes
  straight to `rat_mul_checked`, in the opcode and in `arith_mul`.
  `plain_rat_pair` decides which pairs qualify. A user `infix:<*>` still takes
  the call, and `$*RAT-OVERFLOW` still applies.
- **An i64-sized Rat result is reduced without BigInts.**
  `rat_from_i128_or_num`, used by the checked Rat `+`, `-`, `*` and `/`, built
  two heap `BigInt`s for every result so it could call `make_big_rat_arith`.
  Parts that fit in i64 now reduce through `make_rat`, which gives the same Rat.
  Larger parts, and `i64::MIN`, still take the BigInt path.
- **A `Rat`, `FatRat`, `Complex` or BigInt argument meets its own type in the
  fast accept.** The type check's tag fast path knew `Int`, `Num`, `Str` and
  `Bool`, so a `Rat $r` parameter walked the whole check, through the role-key
  resolution, to accept a `Rat`. That was about 1.4k instructions per bind.
- **The shared `+`, `-` and `*` answer a machine-word Int pair first.** The VM
  opcodes had that fast path, but a `callsame` into the core candidate and a
  reduction fold (`[+]`, `Z*`) call the shared routines directly. There an
  `Int + Int` walked the Range, Date and Duration guards, about 1.1k
  instructions. On the operator section of the same benchmark
  (`multi infix:<*>(UInt, UInt) { callsame() mod $*modulus }`), callgrind went
  from 661.6M to 635.0M instructions.

For the first three, on the multi-method section (30,000 `multi method` calls,
a third of them with a Rat), callgrind went from 936.0M to 851.4M instructions
(-9.0%).

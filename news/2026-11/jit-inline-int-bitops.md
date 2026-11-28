# The JIT computes `+&`, `+|` and `+^` on two small Ints inline

`Add`, `Sub` and `Mul` have had a Tier B inline path since ADR-0004 J4, but the
Int bitwise infixes still called their Tier A shim, which runs the interpreter's
whole `exec_bit_*_op`: the small-Int test, the user-operator probe, then
`int_bitop`. That was about 230 instructions for one `&`, paid per iteration by
loops such as `bench-threads`' `@alias[$i +& 255]` (#19406).

Two small-Int words with no user infix declared in the program (the
`USER_INFIX_DECLS` guard `Add` carries) now take one native instruction. A
bitwise op of two sign-extended 48-bit values is again one, so the result
packs without a range check. Every other operand shape goes to the unchanged
shim. `bench-threads@net+jit` went from 615.0M to 570.4M instructions.
`t/vm/codegen/jit-tier-b-bitops.t` pins small, boundary, big-Int and coercing
operands in hot loops, and a user `infix:<+&>` in a child process.

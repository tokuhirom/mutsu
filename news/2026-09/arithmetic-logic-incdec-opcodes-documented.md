# Arithmetic, logic, coercion and increment opcodes documented

The second slice of the opcode reference (`site/opcodes.html`) adds doc comments
to 37 more `OpCode` variants in `src/opcode.rs` that had none:

- the arithmetic infixes (`Add`, `Sub`, `Mul`, `Div`, `Mod`, `Pow`)
- the prefix operators (`Negate`, `+^`/`?^`/`~^`, `|` slip, `DeSlip`, `Decont`)
- `Not` and `BoolCoerce`
- prefix `+`/`~`/`^` coercion
- the keyword math (`div`, `mod`, `gcd`, `lcm`, `min`, `max`)
- `%%` and `!%%`
- `~`, `x`, `xx` and `o`
- the prefix and postfix `++`/`--` ops, both scalar and indexed

Several of these had only a plain `//` note before, and the page does not show those.

Each doc names the Raku operator the op implements and gives its stack effect.
It also points at the shared primitive that holds the actual semantics:
`crate::builtins::arith_*` and `int_div` in `src/builtins/arith/` (ADR-0118),
`str_prim::concat`/`repeat`, `min_max_values`, and the single
`exec_scalar_incdec_op` body the four scalar `++`/`--` ops share.

Where the dispatch arm shows it, the doc says whether the op threads a Junction
itself. It does for `+`, `div` and `%%`. It does not for `gcd`, `lcm` or prefix
`-`.

Some behaviour was read off the handlers and surprised on inspection. `1/0` is
a zero-denominator `Rat` rather than an immediate Failure. `%%` with a zero
divisor pushes a soft Failure, which `!%%` defuses to `True`. `xx` thunks only a
call-like left operand.

Tracked as #9442.

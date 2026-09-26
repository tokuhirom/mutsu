# Comparison and bitwise opcodes documented

The third slice of the opcode reference (`site/opcodes.html`) adds doc comments
to 38 `OpCode` variants in `src/opcode.rs` that had none:

- the numeric comparisons (`==`, `!=`, `<`, `<=`, `>`, `>=`, `=~=`)
- the six string comparisons
- `before` and `after`
- the three-way ops (`<=>`, `cmp`, `coll`, `unicmp`, `leg`)
- `===`, `!==` and `eqv`
- the integer, boolean and string bitwise ops, and the shifts
- `Isa` and `Does`

Each doc names the Raku operator, gives the stack effect, and names the shared
body the op runs, such as `num_eq_values`, `str_cmp_values`, `identical_values`
or `crate::builtins::int_bitop`. That body is also what the routine form
`&infix:<op>` runs. The three-way ops say that they push an `Order` enum value,
not an `Int`. `=~=` names its tolerance source, `$*TOLERANCE`, which defaults
to `1e-15` relative.

Two of the ops turned out to be unreachable, or nearly so, when checked against
the parser and the `raku` oracle. The docs now say so:

- **`Isa`.** The compiler maps an `isa` infix to it, but Raku has no such infix
  and the parser never produces one. `5 isa Int` is "Two terms in a row" in
  both implementations.
- **`StrShiftLeft` / `StrShiftRight`.** These are reached only through the
  compound assignments `~<=` and `~>=`. The bare `~<` infix does not parse, and
  Rakudo reports it as not yet implemented.

Tracked as #9443.

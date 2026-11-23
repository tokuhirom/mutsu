# TRIR reads a codepoint at a slot or `is rw` position in one op

JSON::Fast's scanners read a character with `nqp::ordat($text, $pos)`, where
`$pos` is a native variable or, in `nom-ws`, the caller's `int $pos is rw`.
TRIR pushed that position onto its int bank with one op and popped it in the
next. The peephole pass now fuses the pair into `OrdAtLocalSlot` or
`OrdAtLocalRef`, which read the position straight from the slot or through the
reference. An inlined callee's reference form becomes its caller's slot form.

The codepoint read itself got cheaper too:

- the per-frame string memo is matched by the text's address, without decoding
  the value;
- the grapheme index answers a flat (ASCII) string inline.

The 100-record SPDX decode went from 56.24M to 54.47M instructions (−3.1%,
callgrind). `t/vm/codegen/adr8673-trir-ordat-fused.t` checks all three shapes
against TRIR off and against rakudo, including positions past either end of a
non-ASCII string.

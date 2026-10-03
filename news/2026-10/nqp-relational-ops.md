# `nqp::` ordered-string, unsigned and string-bitwise ops

Part of the `nqp::` coverage campaign (#11488): the 16 comparison and
bitwise ops tracked by #11491 used to die with `Unsupported nqp:: op`. They
are now implemented:

- the ordered string comparisons `islt_s`, `isle_s`, `isgt_s`, `isge_s`;
- the unsigned comparisons `cmp_u`, `iseq_u`, `isne_u`, `islt_u`, `isle_u`,
  `isgt_u`, `isge_u`;
- `eqatim` / `eqaticim`, which work like `eqat` but ignore marks (and, for
  `eqaticim`, case);
- the codepoint-wise string bit ops `bitand_s`, `bitor_s`, `bitxor_s`.

Each op runs the routine its Raku spelling already runs:

- The `_s` orderings use `str_prim::str_order`, the order behind `lt` and
  `leg`. So `nqp::islt_s("e\x[301]", "f")` is 0, as in MoarVM: the comparison
  is on the NFC form, `é`.
- `eqatim` / `eqaticim` use `nqp_eqat` with the mark folds that
  `indexim` / `indexicim` use.
- The bit ops use the body of `~&`, `~|` and `~^`. In Rakudo those operators
  *are* these ops.

The unsigned ops read an operand's 64 bits as unsigned. A BigInt operand
contributes its low 64 bits, so `nqp::iseq_u(-1, 2**64 - 1)` is 1, as in
MoarVM.

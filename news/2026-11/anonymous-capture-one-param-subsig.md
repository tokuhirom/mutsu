# `| ($x)` stays a capture parameter

An anonymous capture unpacked into a sub-signature with one parameter,
`sub f (| ($x))`, was flattened by the parser into the plain parameter `$x`.
`| ($x, $y)` was already kept as a capture. The flattening had two visible
effects:

- RakuAST lost the capture, so `.DEPARSE` printed `sub f ($x)`.
- The signature reported the parameter's arity (1) instead of the capture's
  (0, count `Inf`), unlike rakudo.

The one-parameter form now takes the same path as the longer ones. Binding
and multi dispatch through the sub-signature are unchanged (roast
`S06-multi/proto.t`). Found through the `.DEPARSE` corpus (#14880).

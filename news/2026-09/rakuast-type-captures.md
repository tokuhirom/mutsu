# RakuAST preserves basic type captures

Basic type-capture parameters such as `::T $value` now round-trip through
mutsu's RakuAST model. The parser and binder already retained the capture in
`ParamDef.type_constraint` and executed it correctly; the missing piece was the
model representation.

## Change

- `RakuAST::Type::Capture` is now registered with its constructor and `name`
  accessor.
- `RakuAST::Parameter` now reads, constructs, and exposes its
  `type-captures` list, including bare `::T` parameters without a fabricated
  target.
- RakuAST lowering rebuilds the existing `::T` parameter metadata and sends it
  through the normal compiler and VM path.

Smiley-constrained and other richer type-capture spellings remain separate
boundaries because the current internal parameter metadata does not preserve
their full RakuAST distinction.

## Coverage

`t/rakuast-type-capture.t` pins the measured read shape, accessors,
construction, positional/named/bare/pointy forms, and EVAL execution under both
mutsu and Rakudo.

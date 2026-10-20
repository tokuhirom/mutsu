# RakuAST preserves source-written names that resemble generated helpers

Source variable references and routine calls beginning with double underscores
now retain their written identity through the RakuAST boundary. The parser uses
the existing source spelling carrier from ADR-12199 only when spelling is
requested; normal execution creates no additional wrapper.

Conversion handles the marked identifier directly, so a user routine named like
an internal helper stays a routine call and a source reference named like an
anonymous temporary stays a lexical reference. Arguments retain their own
provenance: real erased desugarings still fail explicitly. Scalar, array, hash,
code, interpolation, parameter and mutable-capture paths share this behavior.

Declaration-specific anonymous-state name classification remains tracked in
#14014; it is separate from the source-reference and routine-call boundary.

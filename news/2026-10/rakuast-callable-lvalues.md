The RakuAST round-trip frontend now preserves callable assignment targets such
as indexed values, method objects and adverb-bearing subscripts. Parenthesized
declaration assignments retain their source shape, and assignments with one
array target and a Whatever marker reconstruct the written list target while
preserving any skipped RHS prefix.

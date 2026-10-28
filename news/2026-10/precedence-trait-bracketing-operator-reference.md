# `is tighter(&postcircumfix:<[ ]>)` round-trips through RakuAST

The precedence-trait reference check refused any operator name with a space, which the name of a bracketing operator (`postcircumfix:<[ ]>`, `circumfix:<{ }>`) always has, so a routine declared `is tighter(&postcircumfix:<[ ]>)` made the whole unit fail the RakuAST round-trip ("EVAL does not yet support lowering `RakuAST::Sub`"). That kept Math::Matrix from loading; it now passes all 11 of its test files like rakudo. A space is accepted only in a `*circumfix` category.

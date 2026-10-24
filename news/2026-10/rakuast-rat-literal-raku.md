# A RakuAST RatLiteral renders every digit

`RakuAST::RatLiteral.new(1.0).raku` rendered `RatLiteral.new(1)`, and a long
decimal was rounded to `.Str`'s precision, so the `.raku` of a parsed tree did
not round-trip its literal (#14888). A rational leaf now renders as its value's
`.raku`, as rakudo 2026.09 does.

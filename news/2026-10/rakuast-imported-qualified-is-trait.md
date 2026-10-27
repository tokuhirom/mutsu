# RakuAST: `my %h is Pkg::Type` with an imported qualified type round-trips

A variable declaration whose `is` trait names a package-qualified type that the unit imports with
`use` (`my %by-tag is Map::Match = ...` in `Text::Emoji`) was refused by the RakuAST converter with
"declaration with traits (Map::Match)". `bareword::names_type` now also consults the parser's
user-type registry, so an imported type renders as `Trait::Is(type => Type::Simple)` exactly as a
type the unit declares does. Covered by `t/rakuast/rakuast-variable-imported-qualified-type-trait.t`.

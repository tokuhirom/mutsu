# RakuAST: `my %h is Pkg::Type` with an imported qualified type round-trips

A variable declaration whose `is` trait names a package-qualified type that the unit imports with
`use` (`my %by-tag is Map::Match = ...` in `Text::Emoji`) was refused by the RakuAST converter with
"declaration with traits (Map::Match)". `bareword::names_type` now also consults the parser's
user-type registry, so an imported type renders as `Trait::Is(type => Type::Simple)` exactly as a
type the unit declares does. Covered by `t/rakuast/rakuast-variable-imported-qualified-type-trait.t`.

The real `Text::Emoji` (installed through `mzef`) still failed, because the parse-time lookup in an
`inst#` repository only understood `provides` entries of the form `{ "file": ID }`; the layout
`mzef`/zef writes nests it as `{ "lib/X.rakumod": { "file": ID } }`. `find_in_installed_repo` now
reads both, so installed modules' types, constants and exports are visible to the importer's parse
(`t/modules/compunit/installed-repo-nested-provides-type-trait.t`).

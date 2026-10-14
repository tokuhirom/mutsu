# RakuAST: typed enums, colonpair word bodies and computed enum bodies

`.AST` and the `MUTSU_RAKUAST=1` round trip now cover the enum declarations
they used to refuse: `our Str enum E «:A<a> :B<b>»` renders the base type as
`Type::Enum.of`, a `«…»` body with colonpair values renders each pair as a
`QuoteWordsAtom` segment, and a computed parenthesized body
(`enum E (:20A, slip <B C>)`) is kept as the parenthesized term, exactly as
rakudo 2026.09 builds them. Lowering decomposes the term with the parser's own
enum-body decomposition. An `our` enum's values are also known by their
package-qualified name (`Pkg::Text`), and the value names of a computed body
are shared by the converter and the module export scan. 37 more `t/` files
pass under `MUTSU_RAKUAST=1` (19 of them already passed and were not yet
listed).

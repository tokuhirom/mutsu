# An enum body keeps its colonpair members and `<< >>` spacing in `.AST`/`.DEPARSE`

`enum V (:x(1), :y(2))` was rendered as `(x => 1, y => 2)`, and `enum C << a b >>` as `<<a b>>`,
because the parser kept only the normalised `(name, value)` variants. A parenthesized body with a
colonpair member now remembers how each element was written (`EnumVariantForm::WrittenPairs`), and
a `<< >>` / `« »` body of plain words keeps its text (`EnumVariantForm::WrittenQuoteWords`), so the
RakuAST view and `.DEPARSE` match rakudo. A quote body that holds colonpair atoms keeps the
previous (normalised) rendering for now; lowering is unchanged.

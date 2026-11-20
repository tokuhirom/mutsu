# An executing parse builds no spellings

Every parse used to wrap the terms whose source spelling RakuAST keeps
(`<a  b>`, `1 !== 2`, a stray `;`, a heredoc's terminator, `$?LINE`, ...) in
`Expr::Spelled`, and the finalization took each wrapper off again with
`unspell::strip` before the compiler saw the tree. Only `.AST` reads those
spellings, so building them for every executing unit was work done to be undone.

`Expr::spelled` now wraps only while a parse is read as RakuAST: the one flag
`term_origins::Wanted` sets (`ast::spelled::KEEP`) is on for `Str.AST`, slang
productions and the analysis views, and off for every executing unit, every
module load inside them and every lowering. `unspell` is deleted (ADR-15760
§2.5 item 1), an executing parse builds no `Statement::Empty` placeholder for a
`;` that ends no statement, and the parse entry points that hand out the
parser's own tree are named `*_unfinalized` instead of `*_spelled`.

One wrapper carried meaning for execution: `Spelling::ImportedTerm`, the mark
on a bareword the parse resolved as an imported term, which tells the
finalization not to classify the word again (an imported `e` shadows the
setting's). It is now a typed node, `ParsedExpr::ImportedTerm`, built the same
way by both parses (#15760).

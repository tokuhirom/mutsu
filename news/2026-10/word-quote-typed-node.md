# Word quotes are a typed node the parser builds (ADR-15334)

The parser built `qw/a b/`, `«a $b»`, `qqw/a $x/` as calls to the reserved `__mutsu_word_list`,
`__mutsu_words_atom` and `__mutsu_quotewords_atom` routines (with a `__mutsu_qw_literal` pair marker for
glued literal text), wrapped in a `Spelling::WordQuote` / `Spelling::InterpolatingWords` annotation that
`convert` read for the `.AST`, plus a by-name recognizer for the atom calls.

The parser now builds `ParsedExpr::WordList` (the words, `val`, and the quote as written) and
`ParsedExpr::WordsAtom`. The reserved call spellings and the marker exist in one place, `rakuast::word_list`,
which the executing parse (a kept node), `EVAL` of a string and `EVAL` of the `.AST` all lower through.

Deleted: the four `"__mutsu_..."` literals in `src/parser` (`internal-name-literals` 43 -> 39), the two
`Spelling` variants and their `convert` arms, `convert_words_atom` and the parser's duplicate lowering helpers
(`make_*_word_result*`, `quotewords_from_words`, `word_quote_expr`) (`convert-lines` 6886 -> 6875).
Parity test `t/rakuast/rakuast-stage2-word-quote-paths.t`.

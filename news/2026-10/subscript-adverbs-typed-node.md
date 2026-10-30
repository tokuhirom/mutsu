# Subscript adverbs are a typed node the parser builds (ADR-15334, #15401)

`@a[0]:exists`, `%h<a>:delete:k`, `@a[1;2]:kv`, `@a[0]:foo` and their combinations used to be parsed
incrementally into three internal shapes (`Expr::Exists`, a `DELETE-KEY` call, the
`__mutsu_subscript_adverb` / `__mutsu_multidim_*` builtin calls), with the written colonpairs kept
beside them in `Spelled` side channels so `.AST` could read them back through a verified recognizer.

The parser now reads the chain of colonpairs after a subscript once (`scan_subscript_adverbs`, any
colonpair, as Raku does) and builds `ParsedExpr::SubscriptAdverbs { subscript, pairs }`. The node's
view is the `Postcircumfix::*Index(colonpairs => ...)` rakudo has, and the one lowering is
`rakuast::subscript_adverb::lower` (`ast::subscript_adverb::expand` for the built-in adverbs, the
CORE-candidate or user `postcircumfix` call for the others), shared with `EVAL` of a hand-built tree.

Deleted: the parser's ~450-line incremental adverb loop and its `DeleteAdverb` / `collect_remaining_adverbs`
/ `source_adverb_pair` helpers, `named_adverb.rs`, `subscript_adverb::{adverbs, read_back*, decode_mode,
explicit_value_condition, source_pair}`, `rakuast/subscript_adverb/dynamic_multidim.rs`, the
`NamedSubscript` / `ConflictingSubscript` / `ConditionalMultidimDelete` spellings and their `convert` arms,
`l10n_match_adverb`, and twelve `"__..."` literals in `src/parser` (`internal-name-literals` 56 -> 44).
A conflicting value adverb on a multi-dimensional subscript (`@a[0;1]:k:v`) now raises `X::Adverb` instead of
silently dropping the second adverb.

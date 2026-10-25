# The MUTSU_RAKUAST round trip carries lossless families as the parser's tree

Under `MUTSU_RAKUAST=1` every source node used to become an `Arc<RakuAstNode>`, which `lower`
read and freed again. That round trip was the whole remaining cost of the RakuAST frontend
(`#15011`): 1.12-1.20 of the ordinary frontend on `Zef::CLI`, `S32-exceptions/misc.t` and
`emoji-test.t`, against a budget of 1.10 (ADR-10723 section 2.4).

Calls, method calls, literals, variables, operators, interpolation, closures, hashes,
regexes without code, the kept conditional/loop/modifier/assignment constructs and the statements
around them now cross the round trip as one `ParsedLeaf` holding the tree the parser built, which
`lower` hands back (after lowering the kept constructs with their one lowering and dropping the
spelling annotations). `.AST`, `EVAL($ast)` and hand-built trees are unchanged. A spelled parse
notes the ranges of its terms only when `.AST` asked for it.

callgrind Ir of `mutsu -c`, ordinary vs `MUTSU_RAKUAST=1`: `Zef::CLI` 1.16 -> 1.063,
`S32-exceptions/misc.t` 1.16 -> 1.038, `rakuast-source-forms.t` 1.12 -> 1.058, `emoji-test.t`
(all 7,653 lines) 1.052, `Zef::Client` 0.992. The ordinary frontend's count is unchanged.

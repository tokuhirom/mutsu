# The parser's statement-level assignment fallbacks are gone

After `expression()` returned, the statement parser (`simple_expr_stmt::expr_stmt`) re-read `.=`,
`Index = value`, `Index »OP»= value`, `R=`, the set and generic compound assignments on a
non-variable left side and a trailing compound after `($x = ...)`; `parenthesized_assign_expr`
re-read the compound, custom-compound, bracketed-meta and bind forms inside parentheses. The
expression grammar owns all of those now (the assignment family became typed nodes in #14948), and a
line-coverage run of `t/` (both frontends), the roast whitelist and a parse-only pass over every
roast, vendor and batteries file never entered any of these blocks.

About 730 lines go: those blocks, `lvalue_assign_to_expr` and the helpers only they used
(`parse_hyper_assign_op`, `method_lvalue_assign_expr`, `named_sub_lvalue_assign_expr`,
`grouped_assign_lvalue_stmt`). A list of 65 assignment statements prints the same output before and
after, and `t/` (8,715 files) and the roast whitelist pass as before.

The measurement is reusable: `docs/dead-code-coverage.md` has the recipe and
`scripts/coverage-zero-lines.py` lists the runs of lines no test ran.

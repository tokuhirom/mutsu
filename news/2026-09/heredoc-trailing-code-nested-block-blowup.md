# A heredoc with trailing code no longer makes nested blocks exponential

`parse_to_heredoc_with_flags` (`src/parser/primary/string/heredoc.rs`) handles a heredoc marker
line that carries more code after it (`is Q:to[END], 'a', 'x';`) by splicing that trailing code
onto the text after the terminator and `Box::leak`ing the result as the parse's remainder. That
remainder is not a subslice of the string it was parsed from, so `ParseMemo::store`
(`src/parser/memo.rs`) refused to cache it. Every backtracking attempt the statement/expression
parsers made through an enclosing block then re-parsed — and re-leaked — the same heredoc from
scratch, and cost multiplied at every level of block nesting: `ANTLR4::Grammar`'s
`t/10-basic-grammar.t` never finished parsing (`--dump-ast` alone exhausted memory), found while
working #9491.

The memo now admits a remainder that lives in a permanently leaked buffer instead of refusing it
outright: `primary::is_within_leaked_region` confirms the buffer is one `parse_to_heredoc_with_flags`
registered (and therefore never freed), and a new `MemoEntry::OkLeaked` variant records it by raw
pointer/length rather than by an offset into the memoized input. A retried parse over the same
heredoc now hits the cache instead of re-leaking, so the cost is paid once per heredoc occurrence
instead of once per backtracking attempt.

Pinned by `t/lang/quoting/heredoc-trailing-code-nested-blocks-perf.t`: eight levels of nested
`subtest 'n', { BODY BODY }` around such a heredoc (256 heredocs total) used to make `--dump-ast`
take minutes and exhaust memory (depth 4 of the same shape already took 13+ seconds); it now
parses in well under a second.

Closes #9674.

# Statement-initial `{*}` followed by an infix is one expression

In a proto body, `{*} + 7` was split into a bare `{*}` block statement and a separate
`+ 7` statement, dropping the dispatch result. The block-statement parser now treats a
`{*}` block followed by a symbolic infix on the same line as the left operand of the
expression, matching Rakudo's `OnlyStar` term. Regression test:
`t/routines/dispatch/proto-onlystar-infix.t`.

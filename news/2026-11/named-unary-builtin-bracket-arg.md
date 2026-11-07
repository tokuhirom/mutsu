# A named unary builtin takes a following `[` as its argument

`flip [~] 1, 2` printed `flip12`: after `uc`, `lc`, `flip`, `ord`, ... the parser did not treat
`[` as the start of a term, so the word stayed a bareword concatenated with the reduction. A
space-separated `[` now opens the argument (a reduction or array literal), as in rakudo. Found
through Math::Sequences' `lunar-add` (`+ flip [~] (roundrobin ...).map: {.max}`).

# `multi token` candidates in an EVAL'd grammar are not redeclarations

The redeclaration check of `EVAL` (`system_eval_redecl.rs`) put every `token`, `rule` and `regex`
of a package in one namespace and ignored `multi`, so a grammar with `multi token keyw(1) {..}` and
`multi token keyw(2) {..}` died with `Package 'G' already has a regex 'keyw'` inside a string `EVAL`
(a file run was fine). A `multi` candidate may now follow a `proto` or other candidates; a plain
declaration, a second `proto`, or anything after a plain one still collides, as in rakudo (measured
with eight shapes). Found by the #16356 triage; two more `t/grammar` files pass as `EVAL($ast)` and
join the ratchet.

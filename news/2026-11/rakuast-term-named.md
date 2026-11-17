# `RakuAST::Term::Named`: resolution, and a unit's own terms

`now` and `time` in the unit tree `$*CU` now resolve to the setting's
`&term:<now>` / `&term:<time>` (`Declaration::External::Setting`), as on rakudo
2026.09; before, a `Term::Named` had no `resolution` at all.

A unit's own `sub term:<forty-two>` used as `forty-two` is now a
`RakuAST::Term::Named.new("forty-two")` in `.AST` and `$*CU` (it was a
`Call::Name::WithoutParentheses` of `term:<forty-two>`), resolving to the
term's `RakuAST::Sub`; `EVAL` of such a tree calls the term. Part of #14916;
the regression test is `t/rakuast/rakuast-term-named.t`.

# Hand-built NoMatch messages read rakudo's candidates from the table

The `X::Multi::NoMatch` messages mutsu composes itself -- `.grep` without a matcher, `.Int`/`.Numeric`
on a plain object, a `Pair` used as a positional index, `pick`/`roll` on a defined `Bool`, and
`print` on an unhandled `Failure` -- no longer carry hand-copied candidate lists: they read them from
the generated `builtin_method_sigs.tsv` (`builtin_method_sigs::candidate_lines`), so a regeneration
keeps them in step with rakudo. The Failure `print` list was wrong (`(Mu: *%_)`, now `(Mu $:: *%_)`).
`True.pick(2)` and `False.roll` used to say only `No matching candidates for method: pick`; they now
give rakudo's full message. Remainder (3) of #16736.

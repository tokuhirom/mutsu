# Regexes with code blocks are carried through finalization

ADR-15760 slice 2: after the code-free regex declarations (#16580) the regexes that hold code blocks
were the largest class left in the `finalize_unit` fallback (78 of 177 census expressions). They are
carried too, and the code inside is finalized where it stands: for `Expr::RegexLiteral` /
`MatchRegexTree` the walk reaches the `tree`, which the compiler puts on the value; the tree copy a
`token`/`rule`/`regex` term's value holds is finalized and re-attached. The code now keeps the line
information the parser gave it, where the converted copy's blocks carried `SrcSpan::MAX`.

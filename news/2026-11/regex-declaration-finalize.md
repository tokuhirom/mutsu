# Regex declarations without code are carried through finalization

ADR-15760 slice 2, the largest class of the `finalize_unit` fallback census (297 of 466 fallback
expressions): the regex value of a `token`/`rule`/`regex` declaration or term that holds no code
block is kept as the parser built it instead of being converted to a `RegexDeclaration` node and
lowered back. The round trip re-rendered the pattern text (`<[+-]>` became `<[\x[2B] \x[2D]]>`)
and gave the code blocks of the copy no line information, so a carry rule could not be a test of
the output; the regexes with code keep the conversion. Pinned by identity sources in
`finalization_is_the_ordinary_tree` and `t/rakuast/rakuast-regex-declaration-finalize.t`.

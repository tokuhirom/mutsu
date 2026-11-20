# RakuAST origins of block arguments of regex calls

The block arguments of a regex call assertion (`<word(-> $x { 1 })>`,
`<word(:a{ 2 })>`, `<word({ 1 })>`) now carry their `.origin`, with their
signatures, bodies and statements. The arguments are converted from the
fragment the argument text is parsed into for its ranges, which keeps the
statements' positions the regex tree's own copy is parsed without, and those
positions are moved to where the fragment sits in the unit (#15780).

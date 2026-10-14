# RakuAST: regex boundary assertions and other regex-tree gaps

Regexes using `<|b>` / `<|c>` / `<|g>`, `<?|w>` / `<!|w>`, an inline adverb
directly before an atom (`:i<[ab]>`), `*:?`, `@$ref`, a multi-statement
`$( … )`, `<rx={$r}>`, a lookahead over a back-reference, or a regex literal
as a subrule argument (`<v(/\d+/)>`) now keep a regex tree, so they convert
to the RakuAST node rakudo 2026.09 builds and `EVAL` back. The round-trip
frontend ratchet grows by 24 files.

The matcher also gained three fixes found on the way: `<?before <-[\x[22]]>>`
and `<?before <[0..9] + [x]>>` no longer end the lookahead early, and
`/x:i<[ab]>/` applies `:i` instead of ratcheting `x`.

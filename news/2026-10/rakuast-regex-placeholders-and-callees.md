# RakuAST: regex placeholders, scalar callees and package variables

`/$^a/`, `<&$tok(arg)>` / `<&$tok: arg>`, `<@Pkg::list>`, a `:my token T { … }`
written without a `;` at the end of its line, and a `$( rx/<["']>/ )`
interpolation now keep a regex tree, convert to the RakuAST nodes rakudo
2026.09 builds, and `EVAL` back. The round-trip frontend ratchet grows by 9
files.

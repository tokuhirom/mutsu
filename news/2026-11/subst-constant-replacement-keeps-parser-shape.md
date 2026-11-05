# A constant `s///` replacement keeps the parser's shape through the RakuAST lowering

Since the RakuAST frontend became the default, every `s///` replacement came
back from the lowering as `replacement: ""` plus a thunk, so a plain constant
(`s:g/\d+/#/`) was evaluated as a closure once per match with `$/` bound and
materialized (`bench-regex-global`: +68% instructions, +75% allocations, #15157).
The lowering now hands the compiler the parser's own shape for a constant
string replacement with nothing in it a `qq` source would read (no `\`, `$`,
`@`, `%`, `&`, braces or French quotes): the text, and no thunk. An expression,
an interpolation or a compound `s[...] OP= EXPR` keeps the thunk. Note: rakudo's
own `EVAL($code.AST)` of an `s///` loses the replacement text in this release;
mutsu keeps it.

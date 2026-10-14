# RakuAST round-trips `use variables :D`

The type-smiley pragmas (`use variables :D`, `use attributes :_`, ...) now
convert to rakudo's `Pragma(name => "variables", argument =>
ColonPair::True("D"))` instead of being refused, and lower back to the
parser's own `use` statement. Four files that were refused under
`MUTSU_RAKUAST=1` now pass; `t/rakuast/rakuast-smiley-pragma.t` pins the node
shape and that the round-tripped pragma still applies.

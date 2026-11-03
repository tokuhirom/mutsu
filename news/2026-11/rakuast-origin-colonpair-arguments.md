# RakuAST: origins of colonpair arguments

A colonpair written as a call argument or list item (`foo(:b(3), :$x, :!d, :5h, :e<f g>)`) now
carries the range Rakudo gives it, and so do its value (the variable, the number, the word quote)
and the `ArgList` around it. The argument list parser read such pairs through a path that bypassed
the parser's term notes (`colonpair_run_item`), and the value of a variable, numeric or word-quote
pair was never placed. Part of #15780.

# RakuAST: implicit code blocks in a Pod block

An indented paragraph of a delimited Pod block (`=begin pod` ... `=end pod`) is now an
`implicit-code` `Doc::Block` with its `margin`, as rakudo's: the margin is the first line's
indentation, a deeper line keeps what is beyond it, a blank run belongs to the block when more
indented lines follow, and a less indented line starts another block. An indented continuation line
of a text paragraph is joined to it with its indentation dropped. A block holding such text was left
out of the tree before. Part of #14880.

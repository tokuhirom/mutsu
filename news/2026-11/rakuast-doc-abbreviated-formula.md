# An abbreviated `=formula` holds one line, and the lines after it are code

An abbreviated `=formula` block is one line only: the rest of its own line, or
else the next non-blank line, with each run of blanks one space. The parser
used to skip the whole paragraph as Pod, so `=formula E = mc^2` followed by
`say 1;` printed nothing; rakudo runs the `say`. The parser now ends the block
after that line, and `.AST` reads it as a `Doc::Block` (#14880) with the
measured shape (`paragraphs => ("E = mc^2",)`, no trailing newline, a trailing
blank run kept as one space). Text starting with a colon pair or containing a
tab stays out of the tree.

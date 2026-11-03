# RakuAST: Pod blocks are `Doc::Block` statements of `.AST`

A Pod block written between the statements of a unit (`=begin pod ... =end pod`, `=for foo`,
`=head1 Title`, `=item2 text`, nested blocks) is now a `RakuAST::Doc::Block` statement of its
`.AST`, shaped and ranged as rakudo's: a delimited block runs to its `=end`, an extended or
abbreviated one to the next blank line or directive, a paragraph keeps the blank lines after it,
`head1` is a `head` of level 1, and the unit's `StatementList` runs over the blocks. The statement
parser skips Pod, so `pod_blocks.rs` reads the blocks from the source text and places them between
the statements by offset; `EVAL` of such a tree runs nothing for a block. Blocks with configuration,
markup (`B<...>`), indented (implicit code) lines or a verbatim/tabular type (`code`, `table`, ...)
are not read yet and stay out of the tree, as before. Part of #14880.

A `;` inside a comment between statements (`# L<S02/Bare identifiers/"$x.foo;">`) no longer yields a
`Statement::Empty` in `.AST` (the separator run counted every `;`, comments included). That stray
statement used to stand in, by accident, for the Pod block rakudo has at the same index; with the
block now present it shifted every later statement of such a file, which the `ast-text-corpus`
ratchet caught on `roast/S12-methods/calling_syntax.t`.

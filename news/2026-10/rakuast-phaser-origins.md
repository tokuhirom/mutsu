# RakuAST phaser blocks carry their origin

`.AST` now gives the block of a phaser (`BEGIN { ... }`, `END { ... }`,
`LAST { }`, ...) the source positions Rakudo does: its `Block`, `Blockoid` and
`StatementList`, the list running from the first statement to the end of the
last, its `;` included, or sitting just after the `{` when the block is empty.
The phaser itself already had its statement's range.

The parser records the block's positions as it reads the phaser, in every
parse, in a `spans` member of `Stmt::Phaser` (ADR-10723 §4). A phaser over a
bare statement (`INIT say 4`) has no block, and a `FIRST` phaser, which Rakudo
nests in an extra block of its own, keeps mutsu's single block.

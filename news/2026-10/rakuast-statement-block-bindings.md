# RakuAST statement blocks bind their own empty argument aggregates

Evaluating a bare RakuAST Block now executes it with no arguments, including
when its body declares implicit positional or named slurpy placeholders. A
Statement::Expression wrapper follows the same rule. Lowering declares fresh
empty aggregates inside the existing inlined block scope, so a caller's
arguments cannot leak into the block's own placeholders.

Explicit outer lexical reads remain captures, and blocks used as expression
values remain callable. Blocks marked as unable to take a signature reject
placeholder declarations.

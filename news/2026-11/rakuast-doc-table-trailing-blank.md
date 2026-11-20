# A Pod table row ending in a blank stays in the RakuAST tree

The RakuAST view of a Pod `table` gave up on any row that ended in a blank,
so `=table` with a row like `X | O | ` vanished from `.AST.statements` (and
from `.DEPARSE`) while `$=pod` still saw it (#18290). Such rows are now read
as rakudo 2026.11 reads them: the blanks after a final divider are one more
cell, less the divider's own blank (`X | O | ` is `"X", "O", ""`), and
blanks after the text of a last cell stay in that cell (`a | b ` is `"b "`,
in a whitespace table too).

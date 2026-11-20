# `.AST`: a Pod block inside a parenthesised or bracketed list

`my @a = (1,` / `=begin pod` ... `=end pod` / `2);` now yields the `Doc::Block` after the
statement that holds the list (in the unit's statement list or the enclosing block's), as
rakudo 2026.11 does; the same lines inside a string stay text. A block after a binary
operator outside any list and one inside a `q:to` heredoc body remain unhandled (#14880).

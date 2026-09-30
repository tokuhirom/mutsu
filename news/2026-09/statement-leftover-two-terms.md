# Statements no longer split silently at an early parser stop

The statement parser used to return early in a few expression shapes and
leave the rest of the line for the statement list, which took it as further
statements with no separator between them: `{ 42 }() orelse say "x"` ran
`orelse` as a bare word and `say "x"` on its own, `"a" ~~ /a/ eq "x"` left
`eq "x"` behind, and `temp our $out = ''` dropped the `temp`.

Each of those now parses as one statement. A bare-block call such as
`{ ... }()` hands a following operator to the expression parser, a comparison
after a regex smartmatch (`X ~~ /re/ eq Z`) continues with the match as its
left operand, and `temp my/our $x = ...` runs the declaration and then
temporizes the variable (restoring the initializer's value, as rakudo does).

With those early stops gone, the same-line check added for #9918 fires for
any term left on the line, not only an undeclared word infix: a statement
that stops in front of another term is "Two terms in a row", as in rakudo
(#10257).

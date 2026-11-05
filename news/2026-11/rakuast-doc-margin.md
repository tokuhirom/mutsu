# Indented Pod blocks have a `margin`

A Pod block whose directive is indented (`    =begin pod` inside a sub, or at
the top of the unit) is now a `RakuAST::Doc::Block` with `margin` set to that
indentation, its lines read with the margin taken off and its range starting
after it, as rakudo 2026.09 shapes it (#14880; ~450 generated indentations,
forms and contexts agree with `raku`). Before, a delimited block at the top
level read its indented body as implicit code, and one inside a block was
left out.

Two runtime fixes came with it: an abbreviated paragraph (`=head1`, `=item`)
ends at a line indented less than its directive, so the code after it runs
(the parser swallowed it up to the next blank line), and blanks before a Pod
directive on the first line of a unit are still at the start of a line. A
delimited `pod`, `item` or `nested` block drops the blanks of an indented
continuation line, any other measured block keeps them. A margin of exactly
one blank has a paragraph quirk in rakudo that is not read (the block stays
out of the tree).

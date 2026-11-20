# RakuAST: Pod anywhere inside a statement, and not inside a heredoc

Rakudo 2026.11 reads a Pod block written anywhere inside a statement except
quoted text as whitespace, and its RakuAST tree carries it as a `Doc::Block`
right after the statement. #18318 did this for a block inside a parenthesised
or bracketed list; now a block after a binary operator (`my $x = 1 +` /
`=begin pod` ... `2;`), after `=`, or between the arguments of a call
without parentheses is placed the same way (#14880).

A Pod block inside the body of a heredoc (`q:to/END/`) is the heredoc's text:
the RakuAST view no longer hoists it out as a statement.

# RakuAST origins: escaped regex literals, colonpair arguments of regex calls

A regex literal written with backslash-escaped characters (`/a\@b/`) now has
its `.origin`, and so does every node after it in the regex: the walk that
lays regex nodes over their text accepted only a single escaped character.
The colonpair arguments of a regex call assertion (`<word(:a(1), :b, :!c,
:d<x>)>`) and their values are placed too: the argument text is parsed again
as the call the regex tree parsed, so each argument finds the range of the
term it was read from (#15780).

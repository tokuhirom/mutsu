# A hash literal `%(...)` is a term and may be directly subscripted with curly
# braces: `%(a=>1,b=>2){"a"}` is `1`. Previously only the angle-bracket
# postcircumfix `%(...)<a>` worked; the `{...}` postcircumfix was mis-parsed as a
# separate block statement. Whitespace before `{` still parses as a block.
use Test;

plan 8;

is %(a => 1, b => 2){"a"}, 1, 'hash literal brace subscript (a)';
is %(a => 1, b => 2){"b"}, 2, 'hash literal brace subscript (b)';
is %(a => 1, b => 2)<a>, 1, 'angle subscript still works';

my $k = "b";
is %(a => 1, b => 2){$k}, 2, 'hash literal brace subscript with variable key';

is-deeply %(a => 1, b => 2){"a", "b"}, (1, 2), 'hash literal brace slice';

ok %(a => 1, b => 2){"a"}:exists, 'brace subscript :exists (present)';
nok %(a => 1, b => 2){"z"}:exists, 'brace subscript :exists (absent)';

# Nested: subscript result chained further.
is %(a => %(x => 9)){"a"}{"x"}, 9, 'chained brace subscript on nested hash literal';

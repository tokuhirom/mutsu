use Test;

plan 7;

# In q mode a backslash only escapes itself and the active delimiters:
# `q{fo\'o}` keeps the backslash (the DBIish mysql suite pins
# `$dbh.quote(q{fo'o})` against `q{'fo\'o'}`), while `q{a\}b}` drops it.

is q{fo\'o}, "fo\\'o", 'backslash-quote inside braces stays literal';
is q{a\}b}, 'a}b', 'escaped close delimiter drops the backslash';
is q{a\\b}, 'a\\b', 'double backslash collapses to one';
is q/a\/b/, 'a/b', 'escaped symmetric delimiter drops the backslash';
is q{a\nb}, 'a\nb', 'q does not process \n';
is 'fo\'o', "fo'o", 'plain single quotes still unescape the quote';
is q|a\|b|, 'a|b', 'escaped pipe delimiter drops the backslash';

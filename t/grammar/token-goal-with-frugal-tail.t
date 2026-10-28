use Test;

# `open ~ close .*?` in a token: a frugal tail keeps growing under ratchet
# until the closing goal matches (JSON::Hjson's multiline string).
plan 4;

grammar Triple { token TOP { "'''" ~ "'''" .*? } }
is Triple.parse("'''ab\ncd'''").Str, "'''ab\ncd'''", 'a token with a frugal tail after the goal';

grammar Paren { token TOP { "(" ~ ")" .*? } }
is Paren.parse("(ab)").Str, '(ab)', 'the same with parentheses';
nok Paren.parse("(ab"), 'an unclosed token still fails';

grammar Greedy { token TOP { "(" ~ ")" \w* } }
is Greedy.parse("(ab)").Str, '(ab)', 'a greedy tail is unchanged';

use Test;

# Str.chomp($needle) drops $needle from the end when the string ends in it
# (rakudo core; its own RakuAST::Deparse dedents with it).

plan 4;

is "abcxx".chomp("xx"), 'abc', 'drops a trailing needle';
is "ab".chomp("xx"), 'ab', 'leaves a string without it';
is "a\n".chomp("x"), "a\n", 'a needle does not chomp a newline';
is "a\n".chomp, 'a', 'chomp without a needle still drops the newline';

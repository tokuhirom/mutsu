use v6;
use Test;

plan 4;

# Parentheses inside quoted lookahead literals must not affect the outer regex
# parser's nesting depth while it finds the `||` separator.
ok 'p' ~~ / [ [ <?before 'x'> || <?> ] \w ]+ /,
    'a paren-free lookahead literal keeps the sequential alternation';
ok 'p' ~~ / [ [ <?before 'xy'> || <?> ] \w ]+ /,
    'a longer paren-free lookahead literal keeps the sequential alternation';
ok 'p' ~~ / [ [ <?before 'x('> || <?> ] \w ]+ /,
    'a parenthesis in a quoted lookahead literal keeps the sequential alternation';
ok 'p' ~~ / [ [ <?before ':n'> || <?> ] \w ]+ /,
    'a punctuation lookahead literal keeps the sequential alternation';

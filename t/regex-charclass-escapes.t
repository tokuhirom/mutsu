use Test;

plan 6;

# Backslash-escaped brackets inside character classes
nok 'a' ~~ / <[\<\>]> /, 'letter does not match escaped angle bracket char class';
ok '<' ~~ / <[\<\>]> /, 'literal < matches backslash-escaped char class';
ok '>' ~~ / <[\<\>]> /, 'literal > matches backslash-escaped char class';

# Complex character class with multiple escaped brackets and special chars
ok 'a' ~~ / <+ graph - [\<\>\{\}\[\]&=%$]> /, 'graph minus escaped-bracket char class matches letter';
nok '<' ~~ / <+ graph - [\<\>\{\}\[\]&=%$]> /, 'graph minus escaped-bracket char class rejects <';
nok '%' ~~ / <+ graph - [\<\>\{\}\[\]&=%$]> /, 'graph minus escaped-bracket char class rejects %';

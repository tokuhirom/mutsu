use Test;
plan 5;

is ~('ab' ~~ / a <( b || c /), 'b', 'start marker before a sequential alternative';
is ~('ab' ~~ / a <( b | c /), 'b', 'start marker before an unordered alternative';
is ~('ab' ~~ / [ a )> b || c ] /), 'a', 'end marker before a sequential alternative';
is ~('ab' ~~ / [ a <( b )> || c ] /), 'b', 'balanced markers remain valid';
is ~('ab' ~~ / c || a <( b /), 'b', 'start marker in the final branch';

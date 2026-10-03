use v6;
use Test;

# From SION (zef distribution): `[ u '{' (<[0..9A..F]> ** 1..6) '}' | . ]` picked
# the `.` branch because the LTM prefix measure capped `** m..n` at m+1 copies
# and then required the following atom to match right after them.
plan 6;

is ('x12}'  ~~ / [ x ( <[0..9]> ** 1..6 ) '}' | . ] /).Str, 'x12}',  '** 1..6, 2 digits';
is ('x123}' ~~ / [ x ( <[0..9]> ** 1..6 ) '}' | . ] /).Str, 'x123}', '** 1..6, 3 digits';
is ('x123}' ~~ / [ x ( <[0..9]> ** 1..3 ) '}' | . ] /).Str, 'x123}', '** 1..3, 3 digits';
is ('x123456}' ~~ / [ x ( <[0..9]> ** 1..6 ) '}' | . ] /).Str, 'x123456}', '** 1..6, 6 digits';
is ('x123}' ~~ / [ x <[0..9]> ** 1..6 '}' | y ] /).Str, 'x123}', 'no captures';
is ('\u{1F600}' ~~ / \\ [ <[uU]> '{' $<ucs>=( <[0..9A..Fa..f]> ** 1..6 ) '}' | $<c>=( . ) ] /)<ucs>.Str,
   '1F600', 'braced code point escape beats the any-char branch';

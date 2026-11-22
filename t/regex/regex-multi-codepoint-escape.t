use Test;

# A comma-separated list in a regex's `\x[…]` / `\o[…]` is a run of codepoints,
# one atom as a quoted string is (#18737, roast S05-mass/rx.t tests 330, 351).

plan 6;

is ~('abc!!def' ~~ /c \x[21,21] d/), 'c!!d', '\x[21,21]';
is ~('abc!!def' ~~ /c \o[41,41] d/), 'c!!d', '\o[41,41]';
is ~('ab' ~~ /^\x[ 61 , 62 ]/), 'ab', 'whitespace around the numbers';
is ~('abab' ~~ /\x[61,62]**2/), 'abab', 'a quantifier repeats the whole run';
is ~('abbbab' ~~ /\x[61,62]+/), 'ab', '+ does not bind to the last codepoint only';
is ~('abc!def' ~~ /c \x[0021] d/), 'c!d', 'a single codepoint still works';

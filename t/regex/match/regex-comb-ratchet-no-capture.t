use Test;

# `.comb(Regex)` without captures takes the position-only matcher
# (`regex_match_nocap.rs`), which ignored `:r`: a ratcheted `\w+` still gave
# characters back, so `"aaax bbx".comb(/ :r \w+ 'x' /)` found two matches
# where rakudo finds none. The compiled regex engine (ADR-0135) now answers
# for the patterns it covers there too, and it honors ratchet. Expected
# values read off rakudo 2026.07.

plan 6;

is "aaax bbx".comb(/ :r \w+ 'x' /).elems, 0, ':r \w+ does not give back under .comb';
is "aaax bbx".comb(/ \w+ 'x' /).join(","), "aaax,bbx", 'without :r it gives back';
is "a1 b22 c333".comb(/ :r \d+ /).join(","), "1,22,333", ':r \d+ still finds every run';
is "<a><b>".comb(/ '<' .+? '>' /).join(","), "<a>,<b>", 'frugal .+? under .comb';
is "xxyxx".comb(/ :r x+ /).join(","), "xx,xx", ':r x+ under .comb';
is "abab".comb(/ [ab]+ /).join(","), "abab", 'a quantified group under .comb';

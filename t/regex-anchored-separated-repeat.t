use v6;
use Test;

# A `**N` / `**N..M` repeating quantifier carrying a `%` / `%%` separator must
# still honour the separator when the pattern is anchored with a leading `^`.
# Regression: the LTM string expansion folded the leading `^` anchor into the
# quantified atom (`^\d`), which stopped being a "single atom", so the pattern
# was mis-expanded as a bare `%` separator and the separator was silently
# dropped (the anchored form matched contiguous digits instead).

plan 12;

ok "1.2"     ~~ m/^ \d ** 2 % \. $/,     'anchored **2 % . matches 2 dot-separated';
ok "1.2.3.4" ~~ m/^ \d ** 4 % \. $/,     'anchored **4 % . matches 4 dot-separated';
nok "12"     ~~ m/^ \d ** 2 % \. $/,     'anchored **2 % . rejects contiguous';
nok "1.2.3"  ~~ m/^ \d ** 2 % \. $/,     'anchored **2 % . rejects 3 elements';
ok "1,2,3"   ~~ m/^ \d ** 1..3 % \, $/,  'anchored **1..3 % , range matches';
nok "1,2,3,4" ~~ m/^ \d ** 1..3 % \, $/, 'anchored **1..3 % , range rejects 4';
ok "1.2.3"   ~~ m/^ \d ** 3 %% \. $/,    'anchored **3 %% . (trailing allowed) matches';
ok "1.2.3."  ~~ m/^ \d ** 3 %% \. $/,    'anchored **3 %% . matches trailing separator';

# An interpolated array atom quantified with `**N % sep` must PARSE (the
# separator after a placeholder was previously mis-read as a stray `%`).
my @oct = ^256;
ok "1.2.3.4" ~~ m/^ @oct ** 4 % \. $/,   'interpolated array **4 % . matches an IPv4';
nok "1.2.3"  ~~ m/^ @oct ** 4 % \. $/,   'interpolated array **4 % . rejects 3 octets';

# The `+` / `*` anchored separator forms were already correct; keep them green.
ok "a,b,c"   ~~ m/^ \w+ % \, $/,         'anchored + % , still matches';
ok "1.2.3"   ~~ m/^ \d+ %% \. $/,        'anchored + %% . still matches';

done-testing;

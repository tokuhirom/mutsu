use v6;
use Test;

# A scalar interpolated into a regex matches literally even when its value is
# a quote character: the spliced `"` must be escaped or it reads as a
# quoted-literal opener and swallows the rest of the pattern (Text::CSV's
# combine/string quoting: / $q | $e /, s/// with quote vars).

plan 7;

my Str $q = '"';
my Str $e = '"';
my Str $s = ',';

ok '"' ~~ / $e | x /, 'quote-var as FIRST alternation branch matches';
ok 'x' ~~ / $e | x /, 'other branch still reachable';
nok 'y' ~~ / $e | x /, 'non-matching input still fails';
ok '"' ~~ /( $e | $s )/, 'quote-var inside a capture group parses';
ok ',' ~~ / $e | $s | \r | \n /, 'comma matches through the alternation';

my Str $t = 'I said, "Hi!"';
$t.subst-mutate(/( $q | $e )/, { "$e$0" }, :g);
is $t, 'I said, ""Hi!""', 'subst-mutate doubles embedded quotes';

my Str $u = 'plain';
$u.subst-mutate(/( $q )/, { "$e$0" }, :g);
is $u, 'plain', 'no spurious matches on quote-free input';

use v6;
use Test;

# A plain scalar of a pattern with no code block is read at match time as
# well (ADR-12917): the same body serves every value, and `:i`/`:m`, a
# ligature value, a `constant` and an alias behave as the spliced text did.

plan 10;

my $s = "abc";
ok "xabcx" ~~ / $s /, 'plain string';
for <abc xyz> -> $v {
    my $w = $v;
    is so("..$v.." ~~ / $w /), True, "one pattern, value $v";
}
my $n = 42;
ok "a42" ~~ / a $n /, 'number';
ok "ABC" ~~ /:i $s /, ':i';
my $mark = 'ä';
ok "a" ~~ /:m $mark /, ':m strips the value marks';
my $lig = 'ﬆ';
ok "st" ~~ /:i $lig /, ':i folds a ligature value';
constant $c = 'ab';
is ~("ab" ~~ / a | b | $c /), "ab", 'a constant stays in the declarative prefix';
my $q = "o+";
ok "fo+" ~~ / f $q /, 'metacharacters in the value are literal';
my $re = /b+/;
is ~("abbbc" ~~ / a $re c /), "abbbc", 'a Regex value';

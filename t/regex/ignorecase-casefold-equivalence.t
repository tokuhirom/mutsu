use Test;

plan 3;

my $long_s = "\c[LATIN SMALL LETTER LONG S]";

ok $long_s ~~ / :i "s" /,
    ':i quoted literal uses the case-fold spelling for long s';
nok $long_s ~~ / :i <[s]> /,
    ':i character classes retain their character-class semantics for long s';
ok $long_s ~~ / :i <-[s]> /,
    ':i negated character class retains its character-class semantics for long s';

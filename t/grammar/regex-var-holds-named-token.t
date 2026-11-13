use Test;

plan 8;

# `<$var>` where the variable holds a named token/regex (`&name`, a Regex
# routine) interpolates that regex, not the routine's name (URL::Find picks
# its protocol/base tokens into variables and matches `<$protocol>`).

my token t { http }
my $p = &t;
is ~("http" ~~ /<$p>/), 'http', 'a token held in a variable';
nok "t" ~~ /^ <$p> $/, 'not its name';

my token ci {:i http }
my $c = &ci;
is ~("HTTP" ~~ /<$c>/), 'HTTP', 'the token keeps its own :i';

my token alt { [http|ftp] }
my $a = &alt;
is ~("ftp" ~~ /<$a>/), 'ftp', 'an alternation inside the token';

my regex dom { <[a..z .]>+ }
my $d = &dom;
is ~("x http://www.com y" ~~ /<$a> '://' <$d>/), 'http://www.com', 'two tokens in one pattern';

sub pick(:$ascii) { $ascii ?? &t !! &alt }
my $chosen = pick(:ascii);
is "go http now".comb(regex { <$chosen> }).join(','), 'http', '.comb with a chosen token';

my @all = "http://a ftp://b".comb(regex { <$a> '://' <$d> });
is @all.join(' '), 'http://a ftp://b', '.comb finds every match';

my $none = &t;
is ("ftp" ~~ /<$none>/).so, False, 'no match when the token does not match';

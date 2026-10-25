use Test;

plan 9;

# `$x OP= y` on an undefined $x starts from the operator's zero-argument
# value. `mod`, `div` and `gcd` have none, so the assignment dies.

throws-like { EVAL 'my $n mod= 5' }, Exception, message => /'Too few positionals'/,
    'a declaration mod= dies';
throws-like { EVAL 'my $n; $n mod= 5' }, Exception, message => /'Too few positionals'/,
    'mod= on an undefined scalar dies';
throws-like { EVAL 'my $x div= 2' }, Exception, message => /'Too few positionals'/,
    'a declaration div= dies';
throws-like { EVAL 'my $x; $x div= 2' }, Exception, message => /'Too few positionals'/,
    'div= on an undefined scalar dies';
throws-like { EVAL 'my $g gcd= 6' }, X::NoZeroArgMeaning, 'gcd= on an undefined scalar dies';

# A defined target is untouched by the seed.
{
    my $n = 17; $n mod= 5;
    is $n, 2, 'mod= on a defined scalar';
    my $x = 17; $x div= 5;
    is $x, 3, 'div= on a defined scalar';
    my $g = 12; $g gcd= 18;
    is $g, 6, 'gcd= on a defined scalar';
}
{
    my Int $t;
    my $r = (try { $t mod= 3 }) // 'died';
    is $r, 'died', 'a typed-undefined scalar dies too';
}

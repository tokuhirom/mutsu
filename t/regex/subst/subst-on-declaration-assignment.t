use Test;

# From the Color distribution: `( my $out = $type ) ~~ s/d$//` substitutes in
# the freshly declared variable and leaves the source untouched.
plan 4;

my $type = 'rgbd';
( my $out = $type ) ~~ s/d$//;
is $out, 'rgb', 'substitution applies to the declared variable';
is $type, 'rgbd', 'source variable is unchanged';

my $x = 'aXbX';
( my $y = $x ) ~~ s:g/X/-/;
is $y, 'a-b-', 's:g/// on a declaration-assignment';
is $x, 'aXbX', 'source still unchanged';

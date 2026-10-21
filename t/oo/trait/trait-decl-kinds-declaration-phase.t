use Test;

# A user `trait_mod:<is>` runs when the declaration it decorates is compiled,
# for every kind of declaration, so its effect precedes the run-time
# initializers of the unit: each flag below is set back by its initializer
# (`my $flag = 'x'`) after the trait ran. Expected values are the `raku`
# output.

plan 6;

my $sub = 'x';
multi sub trait_mod:<is>(Routine:D $r, :$marked!) { $sub = 'trait' }
sub routine is marked { 1 }
is $sub, 'x', 'a routine trait runs in the declaration phase';

my $var = 'y';
multi sub trait_mod:<is>(Variable:D $v, :$tagged!) { $var = 'trait' }
my $plain is tagged;
is $var, 'y', 'a variable trait runs in the declaration phase';

my $initialized = 'z';
multi sub trait_mod:<is>(Variable:D $v, :$tagged-init!) { $initialized = 'trait' }
my $with-init is tagged-init = 3;
is $with-init, 3, 'a variable with an initializer keeps its value';
is $initialized, 'z', 'and its trait ran in the declaration phase';

my $meth = 'm';
multi sub trait_mod:<is>(Method:D $m, :$flagged!) { $meth = 'trait' }
class WithMethod { method m is flagged { 2 } }
is $meth, 'm', 'a method trait runs in the declaration phase';

my $par = 'p';
multi sub trait_mod:<is>(Parameter:D $p, :$pflag!) { $par = 'trait' }
sub with-param($q is pflag) { }
is $par, 'p', 'a parameter trait runs in the declaration phase';

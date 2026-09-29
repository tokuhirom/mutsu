use Test;

# A bare `$x` after `$^x` in the same block names the placeholder parameter,
# also when the block runs as a map/grep/sequence-operator callback and the
# caller has its own `$x`.

plan 5;

my $x = 100;
my &c = { $^x + $x };
is c(1), 2, 'direct call';
is-deeply (1,).map(&c).List, (2,), 'map callback ignores the caller $x';
is-deeply (1,).grep({ $^x + $x > 50 }).List, (), 'grep callback ignores the caller $x';
is (1, { $^x + $x } ... *)[2], 4, 'sequence generator ignores the caller $x';

sub f { (1, { $^x + $x } ... *)[2] }
sub g { my $x = 100; f() }
is g(), 4, 'sequence generator inside a sub called from a shadowing frame';

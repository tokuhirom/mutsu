use Test;

plan 3;

# `pull-one` over an infinite lazy Seq's iterator is O(1) per element.
my $it = ((* mod 10 < 5) xx *).iterator;
my $t = now;
my $n = 0;
while $n < 20000 { $it.pull-one; $n++ }
is $n, 20000, "20000 pull-one calls complete";
ok now - $t < 10, "pull-one over a lazy source is not quadratic";

my $it2 = (1, 2, 3 ... *).iterator;
is (^5).map({ $it2.pull-one }).List, (1, 2, 3, 4, 5), "a sequence iterator pulls in order";

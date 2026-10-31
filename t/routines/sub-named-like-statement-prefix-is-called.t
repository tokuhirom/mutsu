use Test;

# `race`, `lazy`, `hyper` and `eager` are statement prefixes, not core routines: a
# program that declares its own `sub race(...)` calls it with `race(5)`. mutsu rewrote
# the call to the method form `(5).race` ("No such method 'race' for invocant of type
# 'Int'") before the declared sub was consulted.

plan 9;

sub race(Int $i) { "user race $i" }
sub hyper(Int $i) { "user hyper $i" }
sub lazy(Int $i) { "user lazy $i" }
sub eager(Int $i) { "user eager $i" }

is race(5), 'user race 5', 'a declared sub race is called';
is hyper(5), 'user hyper 5', 'hyper';
is lazy(5), 'user lazy 5', 'lazy';
is eager(5), 'user eager 5', 'eager';
is-deeply (1, 2).map({ race($_) }).list, ('user race 1', 'user race 2'), 'inside a map block';

# The statement prefixes still work in the same program.
my @r = race for 1..3 { $_ * 2 };
is @r.sort.join(','), '2,4,6', 'race for is still the statement prefix';
is (race (1, 2, 3).map({ $_ + 1 })).sort.join(','), '2,3,4', 'race LIST';
is (eager (1, 2)).join(','), '1,2', 'eager LIST';
is (hyper (1, 2)).sort.join(','), '1,2', 'hyper LIST';

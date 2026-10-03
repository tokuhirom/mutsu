use Test;

# `given $junction -> Any $_ { ... }` calls the block with the topic, so a
# Junction autothreads over a parameter whose type rejects it. From
# Benchmark's `ok do given %result.all.value -> Any $_ { ... }`.

plan 4;

is (do given (1|2) -> Any $_ { $_ * 10 }).raku, any(10, 20).raku, 'autothreads';

my @seen;
given (1|2) -> Int $x { @seen.push: $x }
is-deeply @seen.sort.List, (1, 2), 'one call per eigenstate';

given 5 -> Int $x { is $x + 1, 6, 'a plain value binds as before' }

my $j;
given (1|2) -> $x { $j = $x }
ok $j ~~ Junction, 'an untyped parameter takes the Junction itself';

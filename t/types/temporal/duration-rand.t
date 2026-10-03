use Test;

plan 4;

# `Duration` (and `Instant`) do `Real`, whose `rand` is `self.Bridge.rand`.
my $d = Duration.new(10);
isa-ok $d.rand, Num, 'Duration.rand is a Num';
ok 0 <= $d.rand < 10, 'Duration.rand is below the duration';
ok Duration.new(1/2).rand < 0.5, 'a Rat-valued Duration';
isa-ok now.rand, Num, 'Instant.rand is a Num';

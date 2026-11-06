use Test;

# A built-in iterator answers `is-lazy`, `is-deterministic` and
# `is-monotonically-increasing` per kind, as rakudo's does (with or without a
# program-defined `does Iterator` class in scope).
plan 19;
class It does Iterator { method pull-one { IterationEnd } }
my %h = a => 1;
my @cases =
    '1..3' => (1..3).iterator,
    '^5' => (^5).iterator,
    'a..e' => ('a'..'e').iterator,
    '1..*' => (1..*).iterator,
    '5..1' => (5..1).iterator,
    '<a b>' => <a b>.iterator,
    '[1,2]' => [1,2].iterator,
    '(1,2)' => (1,2).iterator,
    '[1,2].lazy' => [1,2].lazy.iterator,
    '(1..*).lazy' => (1..*).lazy.iterator,
    'map' => (1,2).map({ $_ }).iterator,
    'grep' => (1,2).grep({ $_ }).iterator,
    'lazy map' => (1..*).map({ $_ }).iterator,
    'gather' => (gather { take 1 }).iterator,
    '%h' => %h.iterator,
    'comb' => 'abc'.comb.iterator,
    'squish' => (1,1,2).squish.iterator,
    'reverse' => (1,2).reverse.iterator,
    'roll*' => (1,2).roll(*).iterator;
my %expect = (
    '1..3' => '011', '^5' => '011', 'a..e' => '011', '1..*' => '111', '5..1' => '010',
    '<a b>' => '010', '[1,2]' => '010', '(1,2)' => '010', '[1,2].lazy' => '110', '(1..*).lazy' => '111',
    'map' => '000', 'grep' => '000', 'lazy map' => '100', 'gather' => '000', '%h' => '000',
    'comb' => '010', 'squish' => '010', 'reverse' => '010', 'roll*' => '110',
);
for @cases -> $p {
    my $i = $p.value;
    is $i.is-lazy.Int ~ $i.is-deterministic.Int ~ $i.is-monotonically-increasing.Int, %expect{$p.key}, $p.key;
}

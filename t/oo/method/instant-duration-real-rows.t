use Test;

# Instant and Duration `do Real`: each declares the transcendental and rounding
# methods itself, so the method table answers them at the owner (ADR-11276).
plan 12;

my $d = Duration.new(2.5);
my $i = Instant.from-posix(5);

for <sin cos tan sqrt exp log2 log10 sinh asec acotanh floor ceiling sign truncate> -> $m {
    is-deeply $d."$m"(), $d.Num."$m"(), "Duration.$m reads the seconds";
    last if ++$ == 5;
}
is-deeply $d.round, 3, 'Duration.round';
is-deeply $d.round(2), 2.5.round(2), 'Duration.round with a scale';
is-deeply $d.atan2(1), 2.5.atan2(1), 'Duration.atan2 with an argument';
is-deeply $d.log(2), 2.5.log(2), 'Duration.log with a base';
ok Duration.^can('sin'), 'Duration.^can finds sin';
ok Instant.^can('floor'), 'Instant.^can finds floor';
is-deeply $i.floor, $i.Num.floor, 'Instant.floor';

use Test;

# Instants and Durations store their TAI seconds the way Rakudo does: a Rat
# truncated toward zero to whole nanoseconds, whatever built them (#11273).
# Expected values are Rakudo's.

plan 16;

{
    my $n = now;
    is $n.tai.^name, 'Rat', 'now holds a Rat';
    ok $n.tai.denominator <= 1_000_000_000, 'at nanosecond resolution';
    my $d = now - $n;
    is $d.tai.^name, 'Rat', 'Instant - Instant is a Rat-valued Duration';
    is (now - now).narrow.^name, 'Rat', '.narrow of such a Duration';
}

is-deeply Duration.new(1/3).tai.nude, (333333333, 1000000000), 'Duration.new truncates to nanoseconds';
is-deeply Duration.new(-2/3).tai.nude, (-333333333, 500000000), 'toward zero';
is-deeply Duration.new(1e0/3).tai.nude, (333333333, 1000000000), 'a Num argument too';
is-deeply (Duration.new(1/3) + 1/3).tai.nude, (333333333, 500000000), 'Duration + Real';
is-deeply (Duration.new(1) + 1/3).tai.nude, (1333333333, 1000000000), 'Duration + Rat';
is-deeply (Duration.new(5) % 1.5e0).tai.nude, (1, 2), 'Duration % Num';
is (Duration.new(1.5) + 1e0).tai.^name, 'Rat', 'Duration + Num stays a Rat';
is (Duration.new(1.5) - Duration.new(0.25)).raku, 'Duration.new(1.25)', 'Duration - Duration';

is-deeply Instant.from-posix(1/3).tai.nude, (10333333333, 1000000000), 'Instant.from-posix keeps the fraction';
is-deeply (Instant.from-posix(1) + 1/3).tai.nude, (11333333333, 1000000000), 'Instant + Rat';
is-deeply (Instant.from-posix(1/3) - Instant.from-posix(0)).tai.nude, (333333333, 1000000000),
    'Instant - Instant is exact';
is-deeply (Instant.from-posix(10) - 2.5e0).tai.nude, (35, 2), 'Instant - Num';

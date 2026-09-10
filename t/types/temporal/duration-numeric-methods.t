use Test;

plan 16;

# Duration does Real: numeric methods delegate to the underlying value.
my $d = Duration.new(3.5);
is $d.abs, 3.5, 'Duration.abs';
is $d.floor, 3, 'Duration.floor';
is $d.ceiling, 4, 'Duration.ceiling';
is $d.round, 4, 'Duration.round (nearest)';
is $d.truncate, 3, 'Duration.truncate';
is $d.sign, 1, 'Duration.sign (positive)';
is $d.Int, 3, 'Duration.Int';
is $d.Real, 3.5, 'Duration.Real';

my $neg = Duration.new(-2.7);
is $neg.abs, 2.7, 'negative Duration.abs';
is $neg.sign, -1, 'negative Duration.sign';
is $neg.floor, -3, 'negative Duration.floor';
is $neg.ceiling, -2, 'negative Duration.ceiling';

# A Duration is still a Duration (identity methods unaffected).
is $d.^name, 'Duration', 'Duration is still a Duration';
ok $d ~~ Real, 'Duration does Real';

# Difference of two Instants is a Duration whose .abs works.
my $diff = now - now;
ok $diff ~~ Duration, 'now - now is a Duration';
ok $diff.abs >= 0, 'Duration.abs of an Instant difference is non-negative';

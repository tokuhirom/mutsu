use Test;

# A symbolic prefix operator (`+`, `-`, `~`) may be applied to the result of
# ANOTHER prefix operator: `+!$x` is `+(!$x)`. mutsu used to fall through to
# numeric-literal parsing after the leading `+`/`-`/`~` and fail on the `!`.
# (Path::Finder: `return Prune(+!$value)`.)

plan 8;

my $zero = 0;
my $five = 5;

is (+!$zero),     1,     '+!0 numifies the negation (True -> 1)';
is (+!$five),     0,     '+!5 numifies the negation (False -> 0)';
is (+!$zero + 1), 2,     '+!$x participates in further arithmetic';
is (-!$five),     0,     '-!5 negates the numified negation';
is (-!$zero),     -1,    '-!0 is -1';
is (~!$zero),     'True', '~!0 stringifies the negation';
is (?!$five),     False, '?!5 booleanizes the negation (?(!5) = ?False = False)';

# Coercion call wrapping a stacked prefix (the exact Path::Finder shape).
enum Prune <PruneInclusive PruneExclusive>;
sub negate(Prune $v) { return Prune(+!$v) }
is negate(PruneInclusive), PruneExclusive, 'Prune(+!$v) coerces the stacked-prefix result';

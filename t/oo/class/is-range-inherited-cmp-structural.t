use v6;
use Test;

plan 5;

# `cmp` between a Range and an `is Range` instance must compare structurally
# (min, excludes-min, max, excludes-max), the same rules `range_cmp` already
# applies to two native Range values -- not fall back to a type/identity-based
# ordering. Mirrors the Math::Interval shape: a `handles`-delegated `is Range`
# subclass returned from an operator overload, compared against a plain Range
# literal via `cmp ... ~~ Same` (tokuhirom/mutsu#8814).
class Interval is Range {
    has Range $!range is built handles <min max bounds infinite raku gist fmt excludes-min excludes-max>;
}

my $iv = Interval.bless(range => (3..6));
my $plain = 3..6;
is ($iv cmp $plain), Same, 'is Range instance cmp a structurally-equal plain Range is Same';
is ($plain cmp $iv), Same, 'the reverse comparison is also Same';

my $bigger = Interval.bless(range => (3..7));
is ($iv cmp $bigger), Less, 'a smaller max compares Less than a larger one';
is ($bigger cmp $iv), More, 'and the reverse compares More';

my $other_iv = Interval.bless(range => (3..6));
is ($iv cmp $other_iv), Same, 'two structurally-equal is Range instances compare Same';

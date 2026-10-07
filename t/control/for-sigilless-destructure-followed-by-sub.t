use Test;

plan 2;

my @seen;
for (1, 2) Z (3, 4) -> (\p, \q) {
    @seen.push(p + q);
}

sub after-loop($value) { $value + 1 }

is @seen.join(' '), '4 6', 'sigilless destructuring binds both zipped values';
is after-loop(4), 5, 'a sub declaration after the loop parses and runs';

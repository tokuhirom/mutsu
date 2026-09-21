use Test;

# A lazy map/grep pipeline passed to a plain @ parameter must become a lazy
# List view, not be drained by PositionalBindFailover before the sub starts.
sub inspect-lazy-array(@array, Real $limit) {
    my $index = @array.first: :k, * > $limit;
    ($index, @array[^5], @array.^name.Str, @array.is-lazy)
}

plan 1;
is-deeply inspect-lazy-array((1..*).grep(&is-prime), 1000),
    (168, (2, 3, 5, 7, 11), 'List', True),
    'a lazy grep source stays pullable through a plain @ parameter';

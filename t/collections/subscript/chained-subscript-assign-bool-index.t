use Test;

plan 7;

# A chained element store with a Bool or non-integer subscript writes the
# element that index's Int names, as a single subscript already did
# (Geo::Basic's geohash: `@range[$which][not $upper] = $mid`).
my @r = [1, 2], [3, 4];
@r[1][False] = 9;
is-deeply @r, [[1, 2], [9, 4]], 'inner Bool index';
my $upper = 1;
@r[0][not $upper] = 7;
is-deeply @r, [[7, 2], [9, 4]], 'a not-expression index';
@r[True][True] = 0;
is-deeply @r, [[7, 2], [9, 0]], 'Bool on both levels';
@r[0][1.7] = 5;
is-deeply @r, [[7, 5], [9, 0]], 'a Rat index truncates';

my %h;
%h<a>[True] = 3;
is-deeply %h<a>, [Any, 3], 'under a hash key';
my @n;
@n[1][True] = 2;
is-deeply @n, [Any, [Any, 2]], 'vivifies the row';

my @range = [-90, 90], [-180, 180];
my $which = 1;
for ^4 {
    my $mid = @range[$which].sum / 2;
    @range[$which][not $upper] = $mid;
    $which = not $which;
}
is-deeply @range, [[45.0, 90], [90.0, 180]], 'geohash bisection loop';

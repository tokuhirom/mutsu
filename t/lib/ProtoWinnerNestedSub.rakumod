unit module ProtoWinnerNestedSub;

# Trivial protos whose winners declare a `my sub` and call it: the winner
# runs with its own unit's nested-sub table even when the call comes from a
# script that imported an exported wrapper.
proto sub same($, $) {*}
multi sub same($x, $y) {
    my sub compare($a) { $a eq $y }
    compare($x)
}

proto sub lcs(|) {*}
multi sub lcs(@a, @b, &key = -> $x { $x }) {
    my sub compare($x, $y) { key($x) eq key($y) }
    my $i = 0;
    $i++ while $i < @a && $i < @b && compare(@a[$i], @b[$i]);
    $i
}
multi sub lcs(@a, %b) { 'hash' }

sub is-same($x, $y) is export { same($x, $y) }
sub common-prefix(@a, @b) is export { lcs(@a, @b) }
sub common-prefix-fc(@a, @b) is export { lcs(@a, @b, *.fc) }
sub by-hash(@a) is export { lcs(@a, %(a => 1)) }

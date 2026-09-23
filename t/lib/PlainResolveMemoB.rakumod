# Fixture for t/routines/dispatch/plain-sub-resolution-memo.t (#9081).
my sub memo-helper(Int:D $x, int $n) {
    my sub tag() { 'B' }
    tag() ~ ($x * $n)
}
sub run-b(Int:D $x) is export {
    my @r;
    for ^3 -> $i { @r.push: memo-helper($x, $i) }
    @r.join(',')
}

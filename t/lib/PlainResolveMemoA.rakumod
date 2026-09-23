# Fixture for t/routines/dispatch/plain-sub-resolution-memo.t (#9081): a
# compunit-private helper whose name collides with PlainResolveMemoB's and
# with a sub in the test file itself.
my sub memo-helper(Int:D $x, int $n) {
    my sub tag() { 'A' }
    tag() ~ ($x + $n)
}
sub run-a(Int:D $x) is export {
    my @r;
    for ^3 -> $i { @r.push: memo-helper($x, $i) }
    @r.join(',')
}

use Test;

# A thread started while a frame's `my @*x` is visible must not make the
# name-keyed shared store stand in for that dynamic: a later push to `@*x`
# from a block extends the array the frame declared, on this call and on
# every later one (#13732, ADR-12529 §3, slice 44). Checked against rakudo.

plan 4;

sub req() {
    my @*BF;
    await start { 1 };
    { @*BF.push(1) }();
    @*BF.elems
}
is req(), 1, 'a push from a block after a thread started in the frame';
is req(), 1, 'and again on the next call';

sub req2() { my @*BF; (-> { @*BF.push(2) })(); @*BF.elems }
is req2(), 1, 'a later frame of the same name';

sub req3() {
    my @*BF;
    my $m = 'x' ~~ / x <?{ @*BF.push(3); True }> /;
    @*BF.join(',')
}
is req3(), '3', 'a push from a regex assertion';

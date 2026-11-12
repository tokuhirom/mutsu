use Test;

# `name(...)` where `&name` is a parameter or `my &name` of the running frame
# calls that frame's binding: the default, an argument, a rebinding in the
# body -- never a caller's same-named `&name`.

plan 7;

sub order(:&cmp = &[cmp]) { cmp(1, 2) }
is order(), Less, 'the default code parameter is called';
is order(cmp => { $^b <=> $^a }), More, 'a passed code argument is called';

sub caller-side() {
    my &cmp = -> $, $ { 'caller' };
    order()
}
is caller-side(), Less, "a caller's same-named &cmp does not leak in";

sub rebinds() {
    my &f = { 'default' };
    my @r = f();
    &f = { 'rebound' };
    @r.push: f();
    @r
}
is-deeply rebinds(), ['default', 'rebound'], 'a rebinding in the body is seen by the next call';

sub local-my() {
    my &g = { $_ * 3 };
    g(5)
}
is local-my(), 15, 'a my &name in the frame is called';

sub in-loop(:&cmp = &[cmp]) {
    my $more = 0;
    for ^10 -> $i { $more++ if cmp($i, 4) === More }
    $more
}
is in-loop(), 5, 'repeated calls in a loop keep the binding';

sub unset(:&cb) { &cb.defined ?? cb() !! 'none' }
is unset(), 'none', 'an unset optional code parameter stays undefined';

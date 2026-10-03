use Test;

# On a worker thread, a closure's `@a.push` must be visible to a lexical
# (named) sub that captures the same `@a` (#11238). The sub reads it through
# its lexical-sub alias cell, the push lands in the cross-thread atomic lane.
# Found in Cro::WebSocket's MessageSerializer (`@order` in a supply block).

plan 6;

sub outer() {
    my @o; my $n = 0; my %h;
    sub peek() { "{@o.elems} $n {%h.elems}" }
    return -> $m { @o.push($m); $n++; %h{$m} = 1; peek() };
}
my &c = outer();
is (await start { c('a') }), '1 1 1', 'worker: sub sees the closure push';
is c('b'), '2 2 2', 'main thread afterwards sees both pushes';

# A distinct name: the cross-thread lane is keyed by name (#11306).
sub outer2() {
    my @p;
    my &anon = -> { @p.elems };
    sub named() { @p.elems }
    return -> $m { @p.push($m); (anon(), named()) };
}
my &c2 = outer2();
is-deeply (await start { c2(1) }), (1, 1), 'anonymous and named readers agree';

my $in = Supplier.new;
my @seen;
my $s = supply {
    my @order;
    sub peek-order() { @order.elems }
    whenever $in.Supply -> $m { @order.push($m); emit (@order.elems, peek-order()) }
};
$s.tap: { @seen.push($_) };
await start { $in.emit('a'); $in.emit('b'); $in.done };
is @seen.elems, 2, 'two emits';
is-deeply @seen[0], (1, 1), 'supply-block sub sees the first push';
is-deeply @seen[1], (2, 2), 'and the second';

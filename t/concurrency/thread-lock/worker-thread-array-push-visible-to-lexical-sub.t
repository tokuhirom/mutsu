use Test;

# On a worker thread, a closure's `@a.push` must be visible to a lexical
# (named) sub that captures the same `@a` (#11238). The sub reads it through
# its lexical-sub alias cell, the push lands in the cross-thread atomic lane.
# Found in Cro::WebSocket's MessageSerializer (`@order` in a supply block).

plan 10;

sub outer() {
    my @o; my $n = 0; my %h;
    sub peek() { "{@o.elems} $n {%h.elems}" }
    return -> $m { @o.push($m); $n++; %h{$m} = 1; peek() };
}
my &c = outer();
is (await start { c('a') }), '1 1 1', 'worker: sub sees the closure push';
is c('b'), '2 2 2', 'main thread afterwards sees both pushes';

# The same name as `outer`'s array: each routine's `@o` is its own array on a
# worker thread too, not one name-keyed cross-thread lane (#11306).
sub outer2() {
    my @o;
    my &anon = -> { @o.elems };
    sub named() { @o.elems }
    return -> $m { @o.push($m); (anon(), named()) };
}
my &c2 = outer2();
is-deeply (await start { c2(1) }), (1, 1), 'anonymous and named readers agree';

# Sibling routines whose same-named `@o` / `%h` are pushed to from workers.
sub sib-a() { my @o; return -> $m { @o.push($m); @o.elems } }
sub sib-b() { my @o; return -> $m { @o.push($m); @o.elems } }
my &sa = sib-a();
my &sb = sib-b();
# One worker at a time: a push and an `.elems` from two threads would race.
is-deeply (^4).map({ await start { sa($_) } }).List, (1, 2, 3, 4), 'first sibling counts its own pushes';
is-deeply (^3).map({ await start { sb($_) } }).List, (1, 2, 3), 'second sibling counts only its own';
sub sib-h() { my %h; return -> $k { %h{$k} = 1; %h.elems } }
my &ha = sib-h();
my &hb = sib-h();
await start { ha('p') };
is (await start { ha('q') }), 2, 'a hash in one closure clone keeps its keys';
is (await start { hb('r') }), 1, 'and a second clone starts empty';

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

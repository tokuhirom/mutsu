use Test;

# A sigilless term bound to a subscript's or a call's result is bound to
# whatever that expression hands back: an element or accessor container is
# written through, a plain value is readonly -- in a routine, in a loop that
# re-binds it each iteration, and when it is handed on to another routine
# (#17376: mutsu declares such a slot-only term through its env-free lane).

plan 19;

my $map = Map.new((a => 1, b => [1, 2]));
sub rw-it(\c) { c = 99 }
sub isrw($x is rw) { $x = 7 }

sub map-value() {
    my \h = $map<a>;
    my $err = (try { h = 5; 1 }) ?? 'none' !! $!.^name;
    ($err, h, h.VAR.^name)
}
is-deeply map-value(), ('X::Assignment::RO', 1, 'Int'), 'a Map value is readonly';

sub hash-element() {
    my %h = a => 1;
    my \e = %h<a>;
    e = 5;
    my $after-assign = %h<a>;
    rw-it(e);
    ($after-assign, %h<a>, e.VAR.^name)
}
is-deeply hash-element(), (5, 99, 'Scalar'), 'a Hash element is written through, also via a raw parameter';

sub array-element() {
    my @a = 1, 2;
    my \e = @a[1];
    e = 7;
    isrw(e);
    @a
}
is-deeply array-element(), [1, 7], 'an Array element is written through, also via an is rw parameter';

sub value-to-raw() {
    my \h = $map<a>;
    my $raw = (try { rw-it(h); 1 }) ?? 'none' !! $!.^name;
    ($raw, $map<a>)
}
is-deeply value-to-raw(), ('X::Assignment::RO', 1),
    'a readonly term handed to a raw parameter stays readonly';

sub aggregate-value() {
    my \n = $map<b>;
    n.push(3);
    $map<b>
}
is-deeply aggregate-value(), [1, 2, 3], 'a term bound to an Array value mutates it';

sub loop-rebinding() {
    my @kinds;
    for ^3 -> $i {
        my %h;
        my \x = $i == 1 ?? %h<k> !! $map<a>;
        @kinds.push: x.VAR.^name;
    }
    my @writes;
    for ^4 -> $i {
        my %h;
        my \y = $i %% 2 ?? $map<a> !! %h<k>;
        @writes.push: (try { y = 3; 1 }) ?? 'ok' !! 'ro';
    }
    (@kinds.join(','), @writes.join(','))
}
is-deeply loop-rebinding(), ('Int,Scalar,Int', 'ro,ok,ro,ok'),
    'a loop re-binds the term to a value or a container each iteration';

sub incdec() {
    my \h = $map<a>;
    (try { h++; 1 }) ?? 'none' !! $!.^name
}
is incdec(), 'X::Multi::NoMatch', '++ on a readonly term is refused';

class P { has $.v is rw = 3; method m { 4 } }
sub accessor() {
    my $p = P.new;
    my \v = $p.v;
    v = 10;
    my \m = $p.m;
    my $err = (try { m = 1; 1 }) ?? 'none' !! $!.^name;
    ($p.v, $err)
}
is-deeply accessor(), (10, 'X::Assignment::RO'), 'an is rw accessor is written through, a method result is not';

sub f() { 42 }
sub call-result() {
    my \k = f();
    (try { k = 1; 1 }) ?? 'none' !! $!.message
}
is call-result(), 'Cannot modify an immutable Int (42)', 'a sub result is readonly, named by its value';

sub new-element() {
    my %h;
    my \e = %h<new>;
    my \j = e;
    j = 3;
    %h
}
is-deeply new-element(), {new => 3}, 'a term re-bound from a missing element writes it';

sub identity() {
    my @a = 1, 2;
    my $it = @a.iterator;
    my \first = $it.pull-one;
    my \second = $it.pull-one;
    (first, second)
}
is-deeply identity(), (1, 2), 'iterator results bind in order';

sub in-block() {
    my $r;
    for ^2 { my \x := (5, 6)[0]; $r = (try { x = 10; 1 }) ?? 'none' !! $!.message }
    $r
}
is in-block(), 'Cannot modify an immutable Int (5)', 'a block-local term names its value';

my \top = $map<a>;
ok (try { top = 2; 1 }) === Nil, 'a mainline term over a Map value is readonly';
is top, 1, 'and keeps its value';

my %top = k => 1;
my \tel = %top<k>;
tel = 2;
is %top<k>, 2, 'a mainline term over a Hash element writes through';

sub closure-capture() {
    my \h = $map<a>;
    my &c = { h };
    c()
}
is closure-capture(), 1, 'a term captured by a closure reads its value';

sub nil-result() {
    my \m = $map<missing>;
    m =:= Nil
}
ok nil-result(), 'a missing Map key binds Nil';

sub type-object() {
    my %cache = k => Int;
    my \t = %cache<k>;
    my \u = $map<a>.WHAT;
    (t =:= Int, u =:= Int)
}
is-deeply type-object(), (False, True), 'a Hash element is its container, a .WHAT result its type object';

# A routine with a sigilless parameter of the same name as a later term must
# not push that term's declaration off the env-free lane, and a term bound to
# a role-mixin value is stored as is (#17331).
role Marker { }
sub consume(Str \key) { key }
sub cached() {
    my %c = k => (class {}).new but Marker;
    my \key = 'k';
    my \hit = %c{key};
    (hit ~~ Marker, key)
}
is-deeply cached(), (True, 'k'), 'a mixin value binds, beside a same-named parameter elsewhere';

use Test;

# annotations::containers' `Buffer`: `method AT-POS(Int:D $pos) is raw {
# @!buffer.AT-POS: $pos }` hands back a `Proxy` (its `Binder`) bound in an
# inner array. `@buf[0] = 42` must call that Proxy's STORE: `is raw` returns
# the container just as `is rw` does. mutsu routed only an `is rw` accessor
# through the element-container store, so the assignment was dropped.

plan 5;

my class B is Proxy {
    has $!value is default(Nil);
    method new is raw { callwith :&FETCH, :&STORE }
    my method FETCH is raw { $!value }
    my method STORE(Mu $topic is raw --> Nil) { $!value := $topic }
}

my class Buf does Positional {
    has @!inner;
    method add { my $b := B.new; @!inner.BIND-POS: @!inner.elems, $b; $b = @!inner.elems }
    method AT-POS(Int:D $pos) is raw { @!inner.AT-POS: $pos }
    method elems { @!inner.elems }
}

my @buf := Buf.new;
@buf.add; @buf.add;
is @buf[0], 1, 'the slots read back';
@buf[0] = 42;
is @buf[0], 42, 'an assignment through an is raw AT-POS reaches the Proxy STORE';
my $r = (@buf[1] = 7);
is $r, 7, 'the assignment answers the stored value';
is @buf[1], 7, 'and the slot holds it';
is ((@buf[0] = 0) == @buf[0] == 0), True, 'a chained comparison sees the stored value';

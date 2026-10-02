use Test;

# A method that reads and/or assigns an enclosing `my &k` shares one
# container with the declaring scope (#11046).

plan 10;

{
    my &clock = sub { 1 };
    my class A {
        method clock(&new?) { &clock = &new with &new; &clock }
    }
    &clock = sub { 2 };
    is A.clock.(), 2, 'a writing method sees the enclosing reassignment';
    A.clock(sub { 5 });
    is A.clock.(), 5, 'the method reads its own write';
    is clock(), 5, 'the enclosing scope sees the method write';
}

{
    my &clock = sub { 1 };
    my class B {
        method clock() { &clock }
        method set(&n) { &clock = &n }
    }
    sub other() { my &clock = sub { 99 }; B.clock.() }
    &clock = sub { 2 };
    is other(), 2, 'a reading method closes over the declaration scope, not its caller';
    B.set(sub { 3 });
    is clock(), 3, 'a sibling method write reaches the enclosing scope';
    is B.clock.(), 3, 'a sibling method write reaches the reading method';
    is other(), 3, 'still lexical after the sibling write';
}

{
    my &k = -> { 'a' };
    my class K {
        method get { k() }
        method put(&v) { &k = &v }
    }
    K.put(-> { 'b' });
    is K.get, 'b', 'a bare call in a method sees a sibling method write';
    &k = -> { 'c' };
    is K.get, 'c', 'a bare call in a method sees the enclosing reassignment';
    is k(), 'c', 'the enclosing scope keeps its own write';
}

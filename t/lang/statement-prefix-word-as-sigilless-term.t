use v6;
use Test;

# A sigilless term or constant spelled like a statement prefix (`lazy`,
# `eager`, `hyper`, `race`, `sink`, `quietly`) is that term where no
# whitespace follows the word, and the prefix where whitespace does -- the
# rule rakudo's grammar gives (#19364).

plan 12;

my \lazy = (1, 2, 3);
is lazy[1], 2, 'lazy[...] subscripts the term';
is lazy.elems, 3, 'lazy.method calls on the term';
is (lazy), (1, 2, 3), 'a parenthesized bare lazy is the term';

my \eager = (4, 5);
is eager[0], 4, 'eager[...] subscripts the term';

my \hyper = <a b>;
is hyper[1], 'b', 'hyper[...] subscripts the term';

my \race = 9;
is race*2, 18, 'race*2 multiplies the term';

my \sink = 7;
is sink+1, 8, 'sink+1 adds to the term';
sink 3;
is sink, 7, 'sink followed by a blank is still the prefix';

constant quietly = 3;
is quietly.succ, 4, 'quietly.method calls on the constant';
is (quietly 5), 5, 'quietly followed by a blank is still the prefix';

{
    my \lazy = 5;
    throws-like { lazy + 1 }, X::Method::NotFound,
        'lazy followed by a blank is the prefix, so `lazy +1` calls .lazy on 1';
}

is (lazy 1, 2).is-lazy, True, 'the prefix still applies with a blank after it';

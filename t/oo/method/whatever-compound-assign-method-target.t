use Test;

# Found by Red 0.2.7 (t/18-phasers): `.map(*.name ~= "!")` assigns through an
# `is rw` accessor of a type object.  The `*` in the compound-assign target
# primes the whole expression into a WhateverCode.

plan 5;

my @log;
class Foo {
    method name is rw {
        Proxy.new(FETCH => -> $ { "f" }, STORE => -> $, $v { @log.push: $v });
    }
}

my &f = *.name ~= "z";
f(Foo);
is-deeply @log, ["fz"], 'a WhateverCode assigns through the Proxy of a type object';

my @a = 1, 2;
my &g = *[0] += 5;
g(@a);
is-deeply @a, [6, 2], 'an index target primes too';

my &h = * += *;
is &h.arity, 2, 'a bare * target with a primed RHS keeps arity 2';
is &f.arity, 1, 'the method-target WhateverCode has arity one';

@log = ();
Foo.name ~= "y";
is-deeply @log, ["fy"], 'the direct form is unchanged';

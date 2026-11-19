use Test;

# A `my class` / `my role` declared in a `do` block or in a `.map`/`.grep`/
# `.first` callback is lexical to that block: after it, the name means the
# enclosing scope's type again (#18178). The callback runs on the inline loop
# path, and a value-position block reverts its own declarations on exit.

plan 9;

class Inner { method tag { 'outer' } }

my $d = do { my class Inner { method tag { 'do' } }; Inner.tag };
is $d, 'do', 'the do block sees its own class';
is Inner.tag, 'outer', 'a do block does not leak its class';

sub in-routine() { my $v = do { my class Inner { method tag { 'do' } }; 1 }; Inner.tag }
is in-routine(), 'outer', 'nor does one in a routine';

is (1..2).map({ my class Inner { method tag { 'map' } }; Inner.tag }).join(','), 'map,map',
    'the map callback sees its own class';
is Inner.tag, 'outer', 'a map callback does not leak its class';

(1..2).grep({ my class Inner { method tag { 'grep' } }; 1 }).eager;
is Inner.tag, 'outer', 'nor does a grep callback';

(1..2).first({ my class Inner { method tag { 'first' } }; 0 });
is Inner.tag, 'outer', 'nor does a first callback';

role R { method r { 'outer-role' } }
is (1..2).map({ my role R { method r { 'block-role' } }; (class :: does R {}).r }).join(','),
    'block-role,block-role', 'a role declared in the callback';
is (class :: does R {}).r, 'outer-role', 'a map callback does not leak its role';

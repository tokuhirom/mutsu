use Test;

# #15486: a subscript chain rooted at a package stash entry (`Foo::<%reg><x> = 5`)
# stores through the entry's `our` container; it used to die with
# "it returned Stash, not an Array or Hash container".
plan 8;

class Foo { our %reg; our @arr; our %deep; }

Foo::<%reg><x> = 5;
is Foo::<%reg><x>, 5, 'hash entry through a stash subscript';
is-deeply Foo::<%reg>, {:x(5)}, '... lands in the our %reg';

Foo::<%reg><y><z> = 6;
is Foo::<%reg><y><z>, 6, 'a deeper chain autovivifies below the entry';

Foo::<@arr>[2] = 'c';
is-deeply Foo::<@arr>, [Any, Any, 'c'], 'array entry through a stash subscript';

Foo::<%deep><a><b><c> = 1;
is-deeply Foo::<%deep>, {:a(${:b(${:c(1)})})}, 'three levels below the entry';

is-deeply %Foo::reg, {:x(5), :y(${:z(6)})}, 'the same container is reachable as %Foo::reg';

our %g;
OUR::<%g><k> = 3;
is-deeply %g, {:k(3)}, 'OUR::<%g><k> = ...';

Foo::<%reg><list>.push(1);
is-deeply Foo::<%reg><list>, $[1], 'a method on a stash-rooted element';

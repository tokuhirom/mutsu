use Test;

# Found while running the Intl::CLDR suite: a nested class names its enclosing
# class through `constant CUS = Outer`, then reaches the outer class's private
# methods with `$o!CUS::meth`. The alias has to resolve to the real class for
# both the trust check and the owner lookup, in `trusts CUS` as well.

plan 2;

class Outer {
    has $!x = 42;
    class Sel { ... }
    trusts Sel;
    method !x { $!x }
    method sel { Sel.new(:parent(self)) }

    class Sel {
        constant CUS = Outer;
        trusts CUS;
        has Outer $!parent is built;
        method get { $!parent!CUS::x }
    }
}

is Outer.new.sel.get, 42, 'private call through a constant alias of the owner';

class Derived { ... }
class Base {
    constant D = Derived;
    trusts D;
    has $.seen;
    method !note($v) { $!seen = $v; }
}
class Derived is Base {
    method TWEAK { self!Base::note(7) }
}
is Derived.new.seen, 7, 'a `trusts` naming a constant alias admits that class';

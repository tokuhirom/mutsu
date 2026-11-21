use Test;

# Barewords a method resolves through its enclosing packages
# (`package_chain_var_fallback`): the package candidates are probed by
# symbol, in the `our` store, the env and the top-level package table. A
# core type name probed the same way misses every store and stays the type.

plan 8;

module PCT {
    our constant Greeting = 'hi';
    my constant Limit = 3;
    our $counter = 10;
    class Inner::Deep {
        method greet { Greeting }
        method limit { Limit }
        method counter { $counter }
        method bool { Bool }
        method str-name { Str.^name }
        method coerce($x) { Int($x) }
    }
}

my $o = PCT::Inner::Deep.new;
is $o.greet, 'hi', 'our constant of an enclosing module';
is $o.limit, 3, 'my constant of an enclosing module';
is $o.counter, 10, 'our scalar of an enclosing module';
ok $o.bool === Bool, 'a core type name misses the package chain and stays the type';
is $o.str-name, 'Str', 'a core type name used as an invocant';
is $o.coerce('42'), 42, 'a core type name used as a coercer';

module Shadow {
    my constant Bool = 'shadowed';
    class K { method b { Bool } }
}
is Shadow::K.new.b, 'shadowed', 'an enclosing module constant shadows a core type name';

# Repeated reads keep resolving (no stale answer after the first miss).
is (^3).map({ $o.bool.^name }).join(','), 'Bool,Bool,Bool', 'repeated misses stay misses';

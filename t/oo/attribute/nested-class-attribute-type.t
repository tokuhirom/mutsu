use Test;

# A nested class used as the type of an attribute must resolve when the
# enclosing class is constructed. `has Inner $.x` (no explicit default) defaults
# to the `Inner` type object, which is evaluated during construction with no
# active method-class / package context — so the suppressed short name `Inner`
# must still resolve to `Outer::Inner` there. (Regression: this failed with
# "Undeclared name: Inner".) But the short name must remain undeclared OUTSIDE
# the class, matching Raku's lexical scoping for nested classes.

plan 8;

# 1-3: bare nested type as an uninitialized attribute (defaults to the type object).
{
    class Outer {
        class Inner { }
        has Inner $.x;
    }
    my $o = Outer.new;
    ok $o.defined, 'constructed Outer with a nested-class-typed attribute';
    is $o.^name, 'Outer', 'instance has the expected type';
    is $o.x.^name, 'Outer::Inner', 'uninitialized attr defaults to the nested type object';
}

# 4-5: BUILD using the nested type as a parameter default.
{
    class Box {
        class Item is Hash { }
        has Item $.it;
        submethod BUILD(Item :$!it = Item.new) { }
    }
    my $b = Box.new;
    ok $b.it.defined, 'BUILD default constructed a nested-class instance';
    is $b.it.^name, 'Box::Item', 'BUILD default has the nested type';
}

# 6-7: an explicitly-passed nested instance round-trips.
{
    class Reg {
        class Entry { has $.v }
        has Entry $.e;
    }
    my $entry = Reg::Entry.new(v => 42);
    my $r = Reg.new(e => $entry);
    is $r.e.^name, 'Reg::Entry', 'passed nested instance kept its type';
    is $r.e.v, 42, 'passed nested instance kept its value';
}

# 8: the short name stays undeclared OUTSIDE its enclosing class (lexical scope).
{
    class Hidden {
        class Secret { }
    }
    dies-ok { EVAL 'Secret.new' }, 'nested short name is undeclared outside the class';
}

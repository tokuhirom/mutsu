use v6;
use Test;

plan 7;

# `nextsame`/`callsame` inside a role method that was mixed into an instance via
# `but` (or `does`) must fall through to the base object's method of the same
# name. Previously such a `nextsame` found no continuation and returned Nil.

{
    class A1 { method foo { "OH HAI" } }
    role LogFoo { method foo { nextsame } }
    my $x = A1.new but LogFoo;
    is $x.foo, "OH HAI", 'nextsame from a mixed-in role method reaches the base method';
}

{
    class A { method foo { "base" } }
    role R { method foo { "R-" ~ callsame() } }
    my $x = A.new but R;
    is $x.foo, "R-base", 'callsame from a mixed-in role method returns the base result';
}

# A role method that mixes in but does NOT call nextsame still overrides.
{
    class A { method foo { "base" } }
    role R { method foo { "override" } }
    my $x = A.new but R;
    is $x.foo, "override", 'role method without nextsame overrides the base method';
}

# A role method for a *different* name does not disturb the base method.
{
    class A { method foo { "base" } }
    role R { method bar { "rbar" } }
    my $x = A.new but R;
    is $x.foo, "base", 'base method still reachable when the role adds a new method';
    is $x.bar, "rbar", 'role-only method is callable on the mixin';
}

# nextsame can carry a mutated invocant state through to the base method.
{
    class Counter { has $.n is rw = 0; method bump { $!n }
    }
    role Trace { method bump { $.n; nextsame } }
    my $c = Counter.new(n => 5) but Trace;
    is $c.bump, 5, 'nextsame reaches base accessor-like method with state';
}

# callsame result can be post-processed by the role method.
{
    class Shape { method area { 10 } }
    role Doubler { method area { 2 * callsame() } }
    my $s = Shape.new but Doubler;
    is $s.area, 20, 'callsame result is usable in the role method';
}

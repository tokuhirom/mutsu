use Test;

plan 8;

# `bless` must pass its named arguments through to BUILD / TWEAK (as BUILDALL
# does), so a `submethod BUILD(:$!attr!)` with a *required* named parameter
# binds. Previously bless ran BUILD with no args (relying only on folding named
# args into the attribute map), so a required named BUILD param threw
# "Required named parameter not passed". This blocked DBIish, whose
# DBDish::StatementHandle.new does `::?CLASS.bless(|%args)` into a
# `submethod BUILD(:$!parent!, ...)`.

# Direct bless with a literal named arg.
{
    class A {
        has $.parent;
        submethod BUILD(:$!parent!) { }
    }
    is A.bless(:parent('P')).parent, 'P', 'bless(:parent(...)) binds required BUILD param';
}

# Direct bless with a hash splat.
{
    class B {
        has $.parent;
        has $.statement;
        submethod BUILD(:$!parent!, :$!statement) { }
    }
    my %args = parent => 'P', statement => 'S';
    my $b = B.bless(|%args);
    is $b.parent, 'P', 'bless(|%args) binds required param';
    is $b.statement, 'S', 'bless(|%args) binds optional attributive param';
}

# A custom `new(*%args) { ::?CLASS.bless(|%args) }` over a role (the DBIish
# DBDish::StatementHandle shape).
{
    role R {
        has $.parent;
        method new(*%args) { ::?CLASS.bless(|%args) }
    }
    class C does R {
        has $.statement;
        submethod BUILD(:$!parent!, :$!statement, :$rows) { }
    }
    my $c = C.new(:statement('S'), :parent('P'));
    is $c.parent, 'P', 'custom new + bless: required param binds';
    is $c.statement, 'S', 'custom new + bless: optional param binds';
}

# bless still runs TWEAK with the named args.
{
    my $seen;
    class D {
        has $.x;
        submethod TWEAK(:$x) { $seen = $x }
    }
    D.bless(:x(42));
    is $seen, 42, 'bless passes named args to TWEAK';
}

# A missing required BUILD param still throws.
{
    class E {
        has $.parent;
        submethod BUILD(:$!parent!) { }
    }
    dies-ok { E.bless() }, 'missing required BUILD param still dies';
}

# The normal new path is unchanged.
{
    class F {
        has $.parent;
        submethod BUILD(:$!parent!) { }
    }
    is F.new(:parent('P')).parent, 'P', 'new(:parent(...)) still works';
}

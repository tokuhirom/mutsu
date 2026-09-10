use Test;

plan 6;

# A bare statement whose value is a fresh instance of a class with a
# user-defined `sink` method invokes that method in sink context.
{
    my @reg;
    class C {
        has $.n;
        method sink { @reg.push($!n) }
    }
    C.new(n => 1);
    C.new(n => 2);
    is-deeply @reg, [1, 2], 'bare Foo.new statements invoke user method sink';
}

# Sinking happens for a method call returning a fresh instance too.
{
    my $sunk = 0;
    class D {
        method self-ish { self }
        method sink { $sunk++ }
    }
    D.new.self-ish;
    is $sunk, 1, 'method-call chain ending in a fresh instance is sunk';
}

# A value bound into a Scalar container is NOT sunk (Raku container semantics).
{
    my $sunk = False;
    my ($a) = class { method sink { $sunk = True } }.new;
    is $sunk, False, 'my ($a) = ... does not trigger sinking';
    # silence "used once" sink of $a
    $a.defined;
}

# A function-call return is not auto-sunk (it may be an `is rw` container).
{
    my $sunk = False;
    my sub wrap() is rw {
        my $var = class { method sink { $sunk = True } }.new;
        $var;
    }
    wrap();
    is $sunk, False, 'function-call return is not auto-sunk';
}

# A class that is itself a container (defines STORE) is not sunk.
{
    my $sunk = False;
    (class {
        method STORE(*@args) {}
        method foo() { self }
        method sink() { $sunk = True }
    }.new).=foo;
    is $sunk, False, 'STORE-container .=method result is not sunk';
}

# sink runs as a statement, also from within a module-like block scope.
{
    my @log;
    {
        class E { has $.v; method sink { @log.push: $!v } }
        E.new(v => 'x');
        E.new(v => 'y');
    }
    is-deeply @log, ['x', 'y'], 'bare statements inside a block scope are sunk';
}

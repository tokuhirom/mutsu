use v6;
use Test;

plan 15;

# A `:=` rebind decides the variable's writability from what it is bound to
# now: an immutable value refuses assignment, a container writes through.
# mutsu used to record that decision only as a name-keyed readonly mark, which
# is undone when the rebinding routine returns, so a rebind made inside a sub
# left the outer variable with its old writability (#9277). The decision now
# travels on the binding cell every holder of the variable shares
# (ADR-11142 §2.3).

sub msg(&code) {
    try { code() };
    $! ?? $!.message !! 'assigned';
}

# Row 1: rebound to a value inside a sub, the outer variable is immutable.
{
    my $w = 1;
    sub rw() { $w := 42; Nil }
    rw();
    is msg({ $w = 5 }), 'Cannot assign to an immutable value',
        'a value rebind made in a sub makes the outer variable immutable';
    sub aw() { $w = 6 }
    is msg({ aw() }), 'Cannot assign to an immutable value',
        '... for a write from another sub too';
    is $w, 42, '... and the value is the bound one';
}

# Row 3: rebound to a container inside a sub, the variable writes through.
{
    my $y := 5;
    my $z = 10;
    sub rz() { $y := $z; Nil }
    rz();
    is msg({ $y = 3 }), 'assigned', 'a container rebind made in a sub makes the variable writable';
    is "$y $z", '3 3', '... writing through to the bound container';
    sub ay() { $y = 4 }
    is msg({ ay() }), 'assigned', '... for a write from another sub too';
    is "$y $z", '4 4', '... which also reaches the bound container';
    $y++;
    is "$y $z", '5 5', 'an increment writes through as well';
}

# The same rebind made in the rebinding sub itself.
{
    my $q = 1;
    sub rq() { $q := 42; msg({ $q = 5 }) }
    is rq(), 'Cannot assign to an immutable value', 'the rebinding sub refuses the write itself';
    my $t = 1;
    sub rt() { $t := Int; msg({ $t = 5 }) }
    like rt(), /'type object'/, 'a type-object rebind refuses with the type-object error';
}

# Rebinding back and forth: every rebind decides again.
{
    my $d = 1;
    my $e = 2;
    sub rd-value() { $d := 9; Nil }
    sub rd-container() { $d := $e; Nil }
    rd-value();
    rd-container();
    is msg({ $d = 3 }), 'assigned', 'a container rebind after a value rebind is writable';
    is "$d $e", '3 3', '... through the new container';
    rd-value();
    is msg({ $d = 4 }), 'Cannot assign to an immutable value', 'a value rebind after that is immutable again';
}

# A name bound to the old container keeps it.
{
    my $g = 1;
    my $h := $g;
    sub rg() { $g := 100; Nil }
    rg();
    $h = 5;
    is "$h $g", '5 100', 'an alias taken before the rebind keeps the old container';
}

# Rebound to a readonly parameter's value: readonly, as the parameter is.
{
    my $m = 1;
    sub rm($p) { $m := $p; Nil }
    my $src = 3;
    rm($src);
    is msg({ $m = 8 }), 'Cannot assign to a readonly variable or a value',
        'a rebind to a readonly parameter is readonly';
}

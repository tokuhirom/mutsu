use Test;

# #9651: a statement-level `:=` rebind whose source is a bareword naming a
# type, package or term binds to that VALUE -- there is no source variable to
# alias. A bareword that names a sigilless variable or a constant still binds
# to it.

plan 10;

class IB { }

{
    my $s;
    $s := IB;
    ok $s === IB, 'rebind to a class type object';
    $s := Int;
    ok $s === Int, 'rebind to a core type object';
}

{
    # The same bind executed many times (the #9651 benchmark shape).
    my $s;
    my int $i = 0;
    while $i < 100 { $s := IB; $i = $i + 1 }
    ok $s === IB, 'rebind in a loop';
}

{
    my $s = 5;
    $s := IB;
    my $t = 7;
    $s := $t;
    $s = 8;
    is $t, 8, 'a later rebind to a variable still aliases it';
}

{
    my \x = 42;
    my $s;
    $s := x;
    is $s, 42, 'rebind to a sigilless value';
}

{
    my $v = 5;
    my \y := $v;
    my $s;
    $s := y;
    lives-ok { $s = 9 }, 'rebind to a sigilless alias of a container is writable';
    is $v, 9, 'and writes through to the container';
}

{
    constant C = 7;
    my $s;
    $s := C;
    is $s, 7, 'rebind to a constant';
    dies-ok { $s = 1 }, 'a rebound constant is not assignable';
}

{
    sub term:<answer> { 42 }
    my $s;
    $s := answer;
    is $s, 42, 'rebind to a term';
}

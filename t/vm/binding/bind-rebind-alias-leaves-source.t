use Test;

# Rebinding a name that was itself bound to another variable
# (`my $z := $y; $z := 5`) re-points that name only. The variable it was
# bound to keeps its value (#9357). The mirror case, rebinding the SOURCE,
# is t/vm/binding/bind-rebind-leaves-earlier-alias.t (#9207).

plan 12;

{
    my $y = "o2";
    my $z := $y;
    $z := 5;
    is $y, "o2", 'same-scope scalar rebind leaves the old source alone';
    is $z, 5, 'the rebound name holds the new value';
}

{
    my $y = 1;
    my $z := $y;
    $z := 5;
    $y = 9;
    is $z, 5, 'a later write to the old source no longer reaches the rebound name';
}

{
    my $y = 1;
    my $w = 3;
    my $z := $y;
    $z := $w;
    $z = 7;
    is "$y $w $z", "1 7 7", 'rebinding to another variable binds to it, not the old source';
}

sub g {
    my $y = "o2";
    my $z := $y;
    $z := 5;
    "$y $z"
}
is g(), "o2 5", 'rebind inside a routine leaves the old source alone';

{
    my $x = "obj";
    my $*D := $x;
    sub f { $*D := 0 }
    f();
    is $x, "obj", 'a callee rebinding a dynamic variable leaves its old source alone';
    is $*D, 0, 'the dynamic variable holds the new value';
}

{
    my $x = "obj";
    my $D := $x;
    sub h { $D := 0 }
    h();
    is $x, "obj", 'a named sub rebinding a captured lexical leaves its old source alone';
}

{
    my $x = 1;
    my $D := $x;
    my $c = { $D := 0 };
    my $r = { $D };
    $c();
    is "$x $D {$r()}", "1 0 0", 'a closure rebind leaves the old source; siblings see it';
}

{
    my @a = 1, 2;
    my @b := @a;
    @b := [9];
    @b.push(3);
    is-deeply @a, [1, 2], 'array rebind leaves the old source array alone';
}

{
    my %a = a => 1;
    my %b := %a;
    %b := { z => 2 };
    %b<q> = 1;
    is-deeply %a, { a => 1 }, 'hash rebind leaves the old source hash alone';
    is-deeply %b, { z => 2, q => 1 }, 'the rebound hash is the new one';
}

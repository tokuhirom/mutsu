# #9308: `$a := my $z = 2` binds `$a` to `$z`'s container, exactly as
# `my $z = 2; $a := $z` does. The declaration used to reach the rebind as a
# plain value, which was stored into a fresh cell: without a closure the
# frame-slot pairing hid that, but a write from a closure that had captured
# `$a` reached neither name.
use Test;

plan 8;

sub via-closure {
    my $a = 1;
    my &w = { $a = 7 };
    $a := my $z = 2;
    w();
    ($a, $z)
}
is-deeply via-closure(), (7, 7), 'a closure write through $a reaches $z';

sub via-closure-plain {
    my $a = 1;
    my &w = { $a = 7 };
    my $z = 2;
    $a := $z;
    w();
    ($a, $z)
}
is-deeply via-closure-plain(), (7, 7), 'the variable-source form agrees';

sub reverse-write {
    my $a = 1;
    my &w = { $a = 7 };
    $a := my $z = 2;
    $z = 5;
    my $seen = $a;
    w();
    ($seen, $z)
}
is-deeply reverse-write(), (5, 7), 'writes flow both ways';

sub no-closure {
    my $a = 1;
    $a := my $z = 2;
    $a = 7;
    ($a, $z)
}
is-deeply no-closure(), (7, 7), 'the closure-free form still aliases';

{
    my $a;
    $a := my Int $z = 2;
    $z = 3;
    is $a, 3, 'a typed declaration aliases too';
    is $a.VAR.^name, 'Scalar', 'the bound name is still a Scalar container';
}

{
    my $a;
    for 1..3 { $a := my $z = $_ }
    is $a, 3, 'rebinding to a fresh declaration in a loop';
}

{
    my $a;
    is ($a := my $z = 2), 2, 'the bind expression yields the value';
}

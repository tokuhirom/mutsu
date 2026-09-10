use Test;

plan 4;

{
    my $c = 43;
    my $d = 42;

    $_ := $d;
    if $_ := $c { };
    is $_, 43, 'if condition accepts := assignment expression';
}

{
    my $c = 43;
    my $d = 42;
    my $a = 0;

    $_ := $d;
    while ($_ := $c) + $a++ < 44 { };
    is $_, 43, 'while condition accepts parenthesized := expression';
}

{
    $_ = 42;
    if 1 { $_ := 43 };
    is $_, 42, 'if branch body uses block-scoped $_';
}

{
    $_ = 42;
    while 0 { $_ := 43 };
    is $_, 42, 'while body does not leak $_ to outer scope';
}

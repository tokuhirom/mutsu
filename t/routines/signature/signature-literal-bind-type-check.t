use Test;

plan 7;

# `:(Int $a, $b) := capture` binds through the signature binder, so a typed
# parameter is checked (#11542).
{
    my ($a, $b);
    :(Int $a, $b) := \(1, "two");
    is "$a $b", '1 two', 'matching types bind';
}

{
    my ($a, $b);
    throws-like { :(Int $a, $b) := \("x", 2) }, X::TypeCheck::Binding::Parameter,
        'a mismatched positional is refused';
}

{
    my ($a, $b);
    try { :(Int $a, $b) := \("x", 2) }
    like $!.message, /"Type check failed in binding to parameter '\$a'; expected Int but got Str"/,
        'the message names the parameter and types';
}

{
    my ($s);
    throws-like { :(Str :$s) := \(s => 5) }, X::TypeCheck::Binding::Parameter,
        'a mismatched named argument is refused';
}

{
    my ($n);
    :(Str :$n) := \(n => "ok");
    is $n, 'ok', 'a matching named argument binds';
}

{
    my ($a);
    :(Cool $a) := \(42);
    is $a, 42, 'a role/class parent type accepts a subtype';
    my ($c, $d);
    :($c, $d) := \(1, 2);
    is $c + $d, 3, 'untyped parameters are unchanged';
}

done-testing;

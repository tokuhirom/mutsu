use Test;

# `add_dispatchee` on a dispatcher code value grows that dispatcher: every
# alias of it, and a call through it, sees the new candidate (#15371,
# CompUnit::Util's `push-multi`). Checked against rakudo.

plan 7;

{
    my $d := (my multi foo('one') { 'one' }).dispatcher;
    my $alias := $d;
    $d.add_dispatchee(sub ('two') { 'two' });
    is $d('one'), 'one', 'the captured candidate still answers';
    is $d('two'), 'two', 'the added candidate answers a call through the dispatcher';
    is $alias('two'), 'two', 'and through an alias of it';
}

{
    my $d := (my multi bar(Int $) { 'int' }).dispatcher;
    $d.add_dispatchee(sub (Str $) { 'str' });
    is $d('x'), 'str', 'a dispatcher of a typed candidate takes another type';
    is $d(1), 'int', 'without losing its own';
}

{
    my $p := (my proto qux(Numeric) {*});
    $p.add_dispatchee(sub (Int $) { 'qux-int' });
    is $p(1), 'qux-int', 'a candidate-less proto takes a dispatchee';
    ok $p.is_dispatcher, 'and is a dispatcher';
}

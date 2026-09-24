use Test;

# #9207: `my $f := $a` binds `$f` to what `$a` holds at that moment. A later
# `$a := ...` rebinds only `$a`; `$f` keeps the old container. mutsu shared one
# cell between the two names and a later by-name env sync wrote the new value
# through it, re-binding `$f` as well.

plan 12;

{
    my $a := [1];
    my $f := $a;
    $a := [2, 3];
    is-deeply $f, [1], 'mainline: alias keeps the old array after a rebind';
    is-deeply $a, [2, 3], 'mainline: the rebound name holds the new array';
}

sub in-sub {
    my $a := [1];
    my $f := $a;
    $a := [2, 3];
    is-deeply $f, [1], 'sub: alias keeps the old array after a rebind';
    is-deeply $a, [2, 3], 'sub: the rebound name holds the new array';
    $a.push(9);
    is-deeply $a, [2, 3, 9], 'sub: the new array is mutable through the rebound name';
    is-deeply $f, [1], 'sub: mutating the new array does not reach the alias';
}
in-sub();

sub scalar-value {
    my $a := 1;
    my $f := $a;
    $a := 5;
    is $f, 1, 'rebinding to a plain value leaves the alias alone';
    my &c = { $f };
    is c(), 1, 'a closure over the alias sees the old value too';
}
scalar-value();

sub closure-over-alias {
    my $a := [1];
    my $f := $a;
    my &c = { $f };
    $a := [2];
    is-deeply c(), [1], 'a closure over the alias is not re-bound either';
}
closure-over-alias();

{
    my $a = 1;
    my $f := $a;
    $a = 7;
    is $f, 7, 'assignment through a shared container is still seen by the alias';
    $a := 3;
    $f = 9;
    is $f, 9, 'after the rebind the alias still owns the old container';
    is $a, 3, 'and a write to it does not reach the rebound name';
}

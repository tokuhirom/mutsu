use Test;

plan 22;

# `:delete` on a role mixin deletes from the container the mixin wraps.
role R { method greet { 'hi' } }

{
    my %h = a => 1, b => 2;
    my $m = %h but R;
    is $m.greet, 'hi', 'the mixin still answers its role method';
    is $m<a>:delete, 1, ':delete on a hash mixin returns the deleted value';
    nok $m<a>:exists, 'the deleted key is gone from the mixin';
    is $m.elems, 1, 'the mixin lost exactly one entry';
    is $m<b>, 2, 'the surviving entry is untouched';
}

{
    my %h = a => 1, b => 2, c => 3;
    my $m = %h but R;
    is-deeply ($m<a c>:delete).List, (1, 3), 'a slice :delete on a mixin returns every value';
    is $m.keys.List, ('b',), 'a slice :delete removed every named key';
}

{
    my %h = a => 1;
    my $m = %h but R;
    is ($m<zz>:delete).defined, False, ':delete of an absent key on a mixin is undefined';
}

{
    my @a = 1, 2, 3;
    my $m = @a but R;
    is $m[1]:delete, 2, ':delete on an array mixin returns the deleted element';
    nok $m[1]:exists, 'the deleted position is a hole';
    is $m.elems, 3, 'a middle delete leaves the array length alone';
}

# The method form of the same protocol, on a value rather than a named container.
{
    my %h = a => 1, b => 2;
    my $m = %h but R;
    is $m.DELETE-KEY('a'), 1, 'DELETE-KEY on a mixin returns the deleted value';
    is $m.keys.List, ('b',), 'DELETE-KEY on a mixin mutates the wrapped hash';
}

{
    my @a = 1, 2, 3;
    my $m = @a but R;
    is $m.DELETE-POS(0), 1, 'DELETE-POS on a mixin returns the deleted element';
    nok $m[0]:exists, 'DELETE-POS on a mixin mutates the wrapped array';
}

# Deleting the last element trims the array, as it does through `:delete`.
{
    my @a = 1, 2, 3;
    is @a.DELETE-POS(2), 3, 'DELETE-POS returns the last element';
    is-deeply @a, [1, 2], 'DELETE-POS of the last element shortens the array';
}

# A role that supplies the protocol itself wins over the wrapped container.
role Logged {
    method DELETE-KEY($k) { "deleted-$k" }
    method DELETE-POS($i) { "removed-$i" }
}

{
    my %h = a => 1;
    my $m = %h but Logged;
    is $m<a>:delete, 'deleted-a', "a role's own DELETE-KEY handles :delete";
    is %h<a>, 1, "the role's DELETE-KEY left the wrapped hash alone";

    my @a = 1, 2, 3;
    my $ma = @a but Logged;
    is $ma[1]:delete, 'removed-1', "a role's own DELETE-POS handles :delete";
}

# A punned `does Associative` role delegating to a private hash is a mixin too.
role Store does Associative {
    has %!store;
    method AT-KEY($k) { %!store{$k} }
    method ASSIGN-KEY($k, $v) { %!store{$k} = $v }
    method EXISTS-KEY($k) { %!store{$k}:exists }
    method DELETE-KEY($k) { %!store{$k}:delete }
}

{
    my $s = Store.new;
    $s<x> = 42;
    is $s<x>:delete, 42, ':delete on a punned Associative role reaches its DELETE-KEY';
    nok $s<x>:exists, 'the punned role deleted through to its own store';
}

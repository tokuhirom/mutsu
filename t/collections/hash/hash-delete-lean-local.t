use Test;

# `%h{$k}:delete` on a `%` the running code declares takes the lean delete
# through the variable's slot. These pin what it answers and the shapes it
# must leave to the general delete.

plan 18;

sub drop-odd(%in) {
    my %h = %in;
    %h{$_}:delete for %h.keys.grep(* % 2);
    %h
}
is-deeply drop-odd(%(1 => 'a', 2 => 'b', 3 => 'c')), %('2' => 'b'), 'a sub-local delete loop';

{
    my %h = a => 1, b => 2;
    is %h<a>:delete, 1, 'the removed value is returned';
    is-deeply %h, %(b => 2), 'the key is gone';
    my $r = %h<zz>:delete;
    is $r.WHAT, Any, 'an absent key deletes to Any';
    nok $r.defined, '... which is undefined';
}

{
    my %h;
    %h{"key-$_"} = $_ for 1..10;
    %h{"key-$_"}:delete for 1..5;
    is %h.elems, 5, 'interpolated keys';
    is-deeply %h.keys.sort.List, <key-10 key-6 key-7 key-8 key-9>.List, 'the right keys remain';
}

{
    my %h = 1 => 'one', 2 => 'two';
    is %h{1}:delete, 'one', 'an Int key deletes its string form';
    is %h.elems, 1, '... once';
}

{
    my %h is default(42) = a => 1;
    is %h<b>:delete, 42, 'an absent key of a defaulted hash deletes to the default';
}

{
    my %h = a => 1;
    my $x = 5;
    %h<b> := $x;
    is %h<b>:delete, 5, 'a bound element deletes to its value';
    is $x, 5, 'the variable it was bound to is untouched';
    nok %h<b>:exists, 'the bound key is gone';
}

{
    my %h = a => 1, b => 2;
    my %b := %h;
    %b<a>:delete;
    nok %h<a>:exists, 'a delete through a := alias is seen by the original';
}

{
    my %h = a => [1, 2];
    my $v = %h<a>:delete;
    is-deeply $v, [1, 2], 'an Array element deletes to the Array';
}

{
    my Int %h = a => 1;
    is (%h<b>:delete).WHAT, Int, 'an absent key of a typed hash deletes to its type';
}

{
    my %h = a => 1, b => 2, c => 3;
    my @gone = %h<a b>:delete;
    is-deeply @gone, [1, 2], 'a slice delete returns every removed value';
}

{
    my %h = a => 1, b => 2, c => 3;
    my $n = 0;
    $n += $_ for %h.values;
    %h{$_}:delete for <a b>;
    is-deeply %h, %(c => 3), 'a delete after .values iterated the hash';
}

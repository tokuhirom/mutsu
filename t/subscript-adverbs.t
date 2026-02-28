use Test;

plan 8;

{
    my @a = <A B>;
    lives-ok { (@a[0]:p).value = 'x' }, ':p pair value assignment updates array';
    is @a[0], 'x', ':p keeps lvalue semantics';
}

{
    my @a = <A B>;
    lives-ok { (@a[0]:kv)[1] = 'x' }, ':kv second slot assignment updates array value';
    is @a[0], 'x', ':kv keeps lvalue semantics';
}

{
    my @a;
    @a[0] = 42;
    @a[2] = 23;
    is-deeply @a[0,1,2]:p, (0=>42,2=>23), ':p weeds missing sparse indices';
    is-deeply @a[0,1,2]:!p, (0=>42,1=>Any,2=>23), ':!p keeps missing sparse indices';
}

{
    my %h = (0 => 42, 2 => 23);
    is-deeply %h<0 1 2>:kv, (val("0"),42,val("2"),23), 'hash :kv preserves key/value typing';
    is-deeply %h<0 1 2>:!k, <0 1 2>, 'hash :!k keeps requested key order';
}

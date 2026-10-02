use Test;

# #10984: an element bound to a bare value has no container, so `++`/`--`
# die (Raku's rw-only `postfix:<++>` candidates reject it), a later
# `:= $var` rebind gives it a writable container again, and a multi-index
# `BIND-POS`-bound slot refuses a `[;]=` store.

plan 16;

{
    my @b = 1, 2, 3;
    @b[1] := 42;
    throws-like { @b[1]++ }, X::Multi::NoMatch, '@a[i] := literal; @a[i]++ dies';
    throws-like { --@b[1] }, X::Multi::NoMatch, '... and --@a[i] dies';
    is-deeply @b.List, (1, 42, 3), 'the element is unchanged';
}

{
    my %h;
    %h.BIND-KEY("a", 1);
    throws-like { %h<a>++ }, X::Multi::NoMatch, '.BIND-KEY to a value; %h<k>++ dies';
    is %h<a>, 1, 'the entry is unchanged';
}

{
    my @f = 1, 2, 3;
    @f.BIND-POS(1, 42);
    throws-like { @f[1]-- }, X::Multi::NoMatch,
        message => /'postfix:<-->(Int:D)'/, '.BIND-POS to a value; @a[i]-- dies';
    is @f[1], 42, 'the element is unchanged';
}

{
    my @ok = 1, 2, 3;
    @ok[1]++;
    is @ok[1], 3, 'an ordinary element still increments';
}

{
    my @c = 1, 2, 3;
    @c[1] := 7;
    my $x = 5;
    @c[1] := $x;
    lives-ok { @c[1] = 6 }, 'rebinding to a variable makes the element writable';
    is $x, 6, '... and the write reaches the variable';
    @c[1]++;
    is $x, 7, '... as does ++';
}

{
    my @a[2;2];
    @a.BIND-POS(0, 1, 42);
    throws-like { @a[0;1] = 3 }, X::Assignment::RO, 'shaped [;]= into a BIND-POS slot dies';
    is @a[0;1], 42, 'the shaped slot is unchanged';
}

{
    my @b = [1, 2], [3, 4];
    @b.BIND-POS(0, 1, 42);
    throws-like { @b[0;1] = 3 }, X::AdHoc,
        message => 'Cannot assign to an immutable value', 'nested [;]= into a BIND-POS slot dies';
    is @b[0;1], 42, 'the nested slot is unchanged';
    @b[1;0] = 9;
    is @b[1;0], 9, 'another slot still takes [;]=';
}

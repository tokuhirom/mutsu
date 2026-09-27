use Test;

# A deferred `.map` / `.grep` Seq runs its callback over only as much of its
# source as the consumer needs (#9158): `.head(n)`, `.first` and boolification
# pull a prefix, as Rakudo's pull-one iterator does.

plan 23;

{
    my $c = 0;
    my @a = ^100;
    is @a.map({ $c++; $_ * 2 }).head(3), (0, 2, 4), 'map.head result';
    is $c, 3, 'map.head runs the callback three times';
}
{
    my $c = 0;
    my @a = ^100;
    is @a.grep({ $c++; $_ %% 10 && $_ > 0 }).head(2), (10, 20), 'grep.head result';
    is $c, 21, 'grep.head stops at the second match';
}
{
    my $c = 0;
    my @a = ^100;
    is (^100).map({ $c++; $_ }).first, 0, 'map.first on a Range source';
    is $c, 1, '... runs the callback once';
}
{
    my $c = 0;
    my @x = ^10;
    my $s = @x.grep({ $c++; $_ > 0 });
    ok ?$s, 'a grep Seq boolifies';
    is $c, 2, '... after testing just enough elements';
    is $s, (1 .. 9), '... and still yields every element afterwards';
    is $c, 10, '... resuming where it stopped';
    nok ?@x.grep(* > 100), 'an empty grep is false';
    ok !@x.grep(* > 100), '... and its negation true';
}
{
    my $c = 0;
    my @x = ^10;
    my $m = @x.map({ $c++; $_ + 1 });
    ok ?$m, 'a map Seq boolifies';
    is $m.head(3), (1, 2, 3), '... and a consuming .head after it sees the prefix';
}

# A `last` in the callback ends the Seq, whichever pull hits it.
{
    my @j = ^10;
    is @j.map({ last if $_ == 3; $_ }).head(5), (0, 1, 2), 'last inside map, head past it';
    my $q = @j.map({ last if $_ == 2; $_ });
    ok ?$q, 'boolify before the last';
    is $q, (0, 1), '... then the rest stops at the last';
}

# The map reads the Array at pull time, as Rakudo's does.
{
    my @b = 1, 2, 3;
    my $m = @b.map(* + 1);
    @b.push(4);
    is $m, (2, 3, 4, 5), 'map sees a push made before the pull';
    my @d = 1, 2, 3;
    @d = @d.map(* + 1);
    is @d, [2, 3, 4], 'assigning an array its own map';
}

# rw writeback through a prefix pull reaches only the elements pulled.
{
    my @i = 1 .. 6;
    my $r = @i.map({ $_++ });
    is $r.head(2), (1, 2), 'rw map head';
    is @i, [2, 3, 3, 4, 5, 6], '... wrote back only the pulled elements';
}
{
    my @n = 1 .. 4;
    my $g = @n.grep(* > 1);
    ok ?$g, 'a promoting grep boolifies';
    $_ *= 10 for $g.list;
    is @n, [1, 20, 30, 40], '... and still aliases every matched element afterwards';
}

use Test;

# A read of a plain `@`/`%` local (#19286) answers from the slot without the
# by-name probe chain. These pin the shapes where the slot holds a cell, is
# rebound, is captured, or is read from another thread, so the fast read must
# still see the binding the program made.

plan 14;

{
    my @a = 1, 2, 3;
    my %h = a => 1, b => 2;
    is @a[1], 2, 'plain array element read';
    is %h<b>, 2, 'plain hash element read';
}

{
    my @backing = ^8;
    my @alias := @backing;
    @backing[3] = 42;
    is @alias[3], 42, 'a `:=` alias reads a store made through the original';
    @alias[4] = 7;
    is @backing[4], 7, 'a store through the alias is seen through the original';
}

{
    my @a = 1, 2;
    my @b = 10, 20;
    @a := @b;
    is @a[1], 20, 'a rebound array reads its new container';
    my %x = k => 1;
    my %y = k => 2;
    %x := %y;
    is %x<k>, 2, 'a rebound hash reads its new container';
}

{
    my @a = 1, 2, 3;
    my &push-it = { @a.push(4) };
    push-it();
    is @a.elems, 4, 'a closure mutation is seen by the declaring scope';
    is @a[3], 4, 'and its element read';
}

{
    my @a = 1, 2, 3;
    sub reader() { @a[2] }
    @a[2] = 99;
    is reader(), 99, 'a named sub reads the current element of a file-scope array';
}

{
    my @a = ^4;
    my $got = await start { @a[3] };
    is $got, 3, 'a worker thread reads an outer array';
    await start { @a[0] = 100 };
    is @a[0], 100, 'the main thread reads an element stored by a worker';
}

{
    my atomicint $n = 0;
    my @a = 5, 6, 7;
    $n⚛++;
    is @a[2], 7, 'an array read with an atomic variable live in the program';
    my %h = x => 9;
    is %h<x>, 9, 'a hash read with an atomic variable live in the program';
}

{
    my @outer = 1, 2, 3;
    my $sum = 0;
    for ^3 -> $i { $sum += @outer[$i] }
    is $sum, 6, 'a loop reading an array local';
}

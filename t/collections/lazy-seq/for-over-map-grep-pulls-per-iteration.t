use Test;

# A `for` loop over a not-yet-run `.map` / `.grep` Seq pulls one iteration's
# worth of elements at a time (#9936), as Rakudo's `for` pulls its
# iterable's iterator: the callback and the loop body interleave, and a
# `last` leaves the rest of the source unmapped.

plan 20;

{
    my @log;
    for (1..3).map({ @log.push("m$_"); $_ }) { @log.push("b$_") }
    is @log.join(' '), 'm1 b1 m2 b2 m3 b3', 'map callback and loop body interleave';
}
{
    my @log;
    for (1..4).grep({ @log.push("g$_"); $_ %% 2 }) { @log.push("b$_") }
    is @log.join(' '), 'g1 g2 b2 g3 g4 b4', 'grep callback and loop body interleave';
}
{
    my $c = 0;
    my @a = ^1000;
    for @a.map({ $c++; $_ }) { last }
    is $c, 1, '`last` stops the callback after the first element';
}
{
    my @log;
    for (1..6).map({ @log.push("m$_"); $_ }) -> $a, $b { @log.push("b$a$b") }
    is @log.join(' '), 'm1 m2 b12 m3 m4 b34 m5 m6 b56',
        'a two-parameter loop pulls two elements per iteration';
}
{
    my @log;
    for (1..3).map({ @log.push("m$_"); $_ }) -> Int $x { @log.push("b$x") }
    is @log.join(' '), 'm1 b1 m2 b2 m3 b3', 'a typed loop parameter interleaves too';
}
{
    my @got;
    for (1, 2|3, 4).map({ $_ }) -> Int $x { @got.push($x) }
    is @got.join(' '), '1 2 3 4', 'a typed parameter still autothreads a Junction element';
}
{
    my @a = 1..5;
    for @a.map({ $_ *= 10; $_ }) { last if $_ > 20 }
    is @a.join(' '), '10 20 30 4 5', 'a rw map writes back only the elements it reached';
}
{
    my @a = 1..3;
    my @seen;
    for @a.map({ $_++; $_ }) { @seen.push($_) }
    is @seen.join(' '), '2 3 4', 'a rw map yields the mutated elements';
    is @a.join(' '), '2 3 4', '... and writes every one back';
}
{
    my @log;
    my $r = try {
        for (1..3).map({ die "boom" if $_ == 2; $_ }) { @log.push("b$_") }
        1
    };
    nok $r.defined, 'a callback that dies ends the loop';
    is $!.message, 'boom', '... with its exception';
    is @log.join(' '), 'b1', '... after the iterations before it';
}
{
    my $x = 'outer';
    try { for (1..2).map({ die "stop" if $_ == 2; $_ }) -> $x { } }
    is $x, 'outer', 'a named loop parameter is restored when a pull dies';
}
{
    my @r = do for (1..3).map(* * 10) { $_ + 1 };
    is @r.join(' '), '11 21 31', 'a collecting loop collects each iteration';
}
{
    my $c = 0;
    my $s = (1..5).map({ $c++; $_ * 2 });
    for $s<> { last if $_ == 4 }
    is $c, 2, 'a loop over a stored map Seq pulls only what it iterates';
}
{
    my $c = 0;
    my \s = (1..5).map({ $c++; $_ });
    for s { last }
    is $c, 1, 'a loop over a sigilless-bound map Seq pulls one element for `last`';
}
{
    my $c = 0;
    my @x = ^10;
    my $s = @x.grep({ $c++; $_ > 2 });
    ok ?$s, 'a boolified grep Seq pulled a prefix';
    my @got;
    for $s<> { @got.push($_) }
    is @got.join(' '), '3 4 5 6 7 8 9', '... and a later loop starts from that prefix';
    is $c, 10, '... running the callback over the rest exactly once';
}
{
    my @got = gather for (1..4).map({ $_ * 2 }) { take $_ };
    is @got.join(' '), '2 4 6 8', 'a loop inside a gather over a map Seq';
}

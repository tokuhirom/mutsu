use Test;

# A sigilless parameter of a `.map`/`.grep`/`.first` block (`-> \x`) is
# readonly only inside that block. Its readonly marker must not outlive the
# loop: a later, unrelated `$x is rw` parameter stays writable (#11429).

plan 7;

{
    my @f = 1, 2;
    @f.map(-> \x { x }).eager;
    my @a = 1, 2, 3;
    @a.map(-> $x is rw { $x *= 10 }).eager;
    is-deeply @a, [10, 20, 30], 'map: a later `$x is rw` map block can write';
}

{
    (1, 2).map(-> \x { x }).eager;
    my $v = 1;
    sub g($x is rw) { $x *= 10 }
    g($v);
    is $v, 10, 'map: a later sub with `$x is rw` can write';
}

{
    (1, 2).grep(-> \x { x }).eager;
    my $v = 2;
    sub h($x is rw) { $x *= 10 }
    h($v);
    is $v, 20, 'grep: a later sub with `$x is rw` can write';
}

{
    (1, 2).first(-> \x { x });
    my $v = 3;
    sub k($x is rw) { $x *= 10 }
    k($v);
    is $v, 30, 'first: a later sub with `$x is rw` can write';
}

{
    my @f = 1, 2;
    @f.map(-> \x { x }).eager;
    my @b = 1, 2;
    for @b -> $x is rw { $x *= 10 }
    is-deeply @b, [10, 20], 'a later `for ... -> $x is rw` can write';
}

{
    my \x = 5;
    (1, 2).map(-> \x { x }).eager;
    dies-ok { x = 6 }, 'an enclosing sigilless `x` stays readonly after the loop';
}

dies-ok { (1, 2).map(-> \x { x = 5 }).eager },
    'the sigilless param is still readonly inside the block';

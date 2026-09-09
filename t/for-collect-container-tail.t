use Test;

plan 9;

# A Raku block's value is not decontainerized, so a value-collecting `for`
# whose body ends in a bare variable read gathers that variable's *container*.
# Every collected slot therefore reads it once the loop is over -- after the
# last iteration's `temp` restore.
# https://github.com/tokuhirom/mutsu/issues/7718
#
# The per-iteration `temp` restore itself is #7677's, pinned by
# t/loop-body-let-resolution.t; what is pinned here is what the loop collects.

{
    my $g = 1;
    my @v = do for 1..2 { temp $g = 9; $g };
    is-deeply @v, [1, 1], 'a temp-ed container is collected, not its in-block value';
}

{
    my $g = 1;
    my @v = do for 1..2 { temp $g = 9; $g + 0 };
    is-deeply @v, [9, 9], 'decontainerizing the tail opts back out';
}

{
    my $g = 1;
    my @v = do for 1..2 { $g };
    $g = 5;
    is-deeply @v, [1, 1], 'the container is read when the list is built, not later';
}

{
    my $g = 1;
    my @v = do for 1..2 { $g = $g + 1; $g };
    is-deeply @v, [3, 3], 'every slot holds the same container, so every slot reads 3';
}

{
    our $o = 1;
    my @v = do for 1..2 { temp $o = 9; $o };
    is-deeply @v, [1, 1], 'an `our` container collects the same way a `my` one does';
}

# `state` storage is one cell for the whole loop, so it collects as one.
{
    my @v = do for 1..3 { state $s = 0; $s = $s + $_; $s };
    is-deeply @v, [6, 6, 6], 'a state tail is one container across the iterations';
}

# A container that is rebound per iteration is NOT shared: the loop parameter,
# the topic, and a `my` declared in the body each get a fresh one.
{
    my @v = do for 1..3 -> $i { $i };
    is-deeply @v, [1, 2, 3], 'the loop parameter is rebound per iteration';
}

{
    my @v = do for 1..3 { $_ };
    is-deeply @v, [1, 2, 3], 'the topic is rebound per iteration';
}

{
    my @v = do for 1..3 { my $x = $_ * 2; $x };
    is-deeply @v, [2, 4, 6], 'a body-local `my` is a fresh container per iteration';
}

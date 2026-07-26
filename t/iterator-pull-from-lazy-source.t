use Test;

plan 12;

# `.iterator` on a lazy value snapshots whatever prefix the source has produced
# so far — for a `gather` that has never been forced, nothing. The protocol
# methods then stepped straight past the end and answered the `IterationEnd`
# sentinel as an ordinary value (DBIish t/05-mock.rakutest test 12). They pull
# from the source on demand now, and the pull stays bounded so an infinite
# source is still safe.

{
    my $s = gather { take 1; take 2; take 3 };
    is $s.iterator.pull-one, 1, 'pull-one on an unforced gather yields the first element';
}

{
    my $s = gather { take 'a'; take 'b'; take 'c' };
    my $i = $s.iterator;
    is $i.pull-one, 'a', 'first pull';
    is $i.pull-one, 'b', 'second pull';
    is $i.pull-one, 'c', 'third pull';
    is $i.pull-one, IterationEnd, 'the sentinel arrives only at the real end';
}

# Elements can be any value, including a list (the DBIish row shape).
{
    class Rows { method allrows() { gather { take ['a', 'b', 1]; take ['d', 'e', 2] } } }
    my $rows = Rows.new.allrows;
    is $rows.^name, 'Seq', 'gather in a method still returns a Seq';
    is-deeply $rows.iterator.pull-one, ['a', 'b', 1], 'the first row comes back whole';
}

# An infinite source must stay lazy: the pull is bounded by what is needed.
{
    my $s = gather { my $n = 0; loop { take $n++ } };
    my $i = $s.iterator;
    is $i.pull-one, 0, 'pull-one on an infinite gather does not force it';
    is $i.pull-one, 1, 'and can be pulled again';
}

# The rest of the index-advancing family sees the pulled elements too.
{
    my $s = gather { take 10; take 20; take 30 };
    my @out;
    $s.iterator.push-exactly(@out, 2);
    is-deeply @out, [10, 20], 'push-exactly pulls what it was asked for';
}
{
    my $s = gather { take 5; take 6 };
    my @out;
    $s.iterator.push-all(@out);
    is-deeply @out, [5, 6], 'push-all drains a finite lazy source';
}
{
    my $s = gather { take 7; take 8; take 9 };
    my $i = $s.iterator;
    $i.skip-one;
    is $i.pull-one, 8, 'skip-one advances through the lazy source';
}

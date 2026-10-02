use Test;

plan 11;

# `Seq.new($iterator)` copied a built-in Iterator's whole backing array, so
# elements an earlier `pull-one`/`skip-one` had already consumed came back
# again (#10845). The Seq now pulls from the iterator's current cursor, like
# Rakudo's.

{
    my $i = (1, 2, 3).iterator;
    $i.pull-one;
    is-deeply Seq.new($i).List, (2, 3), 'Seq.new starts at the cursor after pull-one';
}

{
    my $i = (1..5).iterator;
    $i.skip-one;
    $i.skip-one;
    is Seq.new($i).elems, 3, 'Seq.new starts at the cursor after skip-one';
}

{
    my $i = (1, 2, 3).iterator;
    my $s = Seq.new($i);
    is $i.pull-one, 1, 'pulling the iterator after Seq.new still works';
    is-deeply $s.List, (2, 3), 'and the Seq shares the iterator, as in Rakudo';
}

is Seq.new((1, 2, 3).iterator).gist, '(1 2 3)', 'gist of a fresh Seq.new($iterator)';

{
    class CountTo3 does Iterator {
        has $.n = 0;
        method pull-one { $!n < 3 ?? $!n++ !! IterationEnd }
    }
    is Seq.new(CountTo3.new).gist, '(0 1 2)', 'gist pulls a user iterator';
    my $s = Seq.new(CountTo3.new);
    is $s.gist, '(0 1 2)', 'gist of a stored Seq.new over a user iterator';
}

# `say` renders a not-yet-pulled Seq.new($iterator) by pulling it; it printed
# `()` before.
{
    my $out = '';
    my $*OUT = class { method print(*@a) { $out ~= @a.join }; method flush {} }.new;
    say Seq.new((4, 5).iterator);
    $*OUT = $PROCESS::OUT;
    is $out, "(4 5)\n", 'say pulls a deferred Seq.new($iterator)';
}

# A Seq over a lazy iterator is lazy, and gists as Rakudo's placeholder rather
# than pulling forever.
{
    my $s = Seq.new((1..*).iterator);
    ok $s.is-lazy, 'Seq.new over a lazy iterator is lazy';
    is $s.gist, '(...)', 'and gists as (...)';
    is-deeply $s.head(3).List, (1, 2, 3), 'while head still pulls from it';
}

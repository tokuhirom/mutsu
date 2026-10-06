use v6;
use Test;

# `Supply.tail` on a live (Supplier-backed) supply is a pipeline stage (#11839):
# it holds back the last N emitted values and releases them -- then its own
# `done` -- when the source is done. It used to slice the source's (still empty)
# snapshot at call time, so nothing was ever emitted. `.first(:end, ...)` is
# rakudo's `.grep(|c).tail` and rides the same stage.
#
# Every expectation below was verified against Rakudo.

plan 18;

# Runs `$build` on a fresh Supplier's Supply, emits 1..$n, finishes the
# supplier, and returns what the derived supply tapped plus whether it saw done.
sub run(&build, Int $n = 3) {
    my $s = Supplier.new;
    my @got;
    my $done = False;
    build($s.Supply).tap({ @got.push($_) }, done => { $done = True });
    $s.emit($_) for 1..$n;
    $s.done;
    (@got.List, $done);
}

my ($got, $done);

($got, $done) = run({ .tail(1) });
is-deeply $got, (3,), 'tail(1) emits the last value at done';
ok $done, '... and then finishes';

($got, $done) = run({ .tail }, 4);
is-deeply $got, (4,), 'tail with no argument is tail(1)';

($got, $done) = run({ .tail(3) }, 5);
is-deeply $got, (3, 4, 5), 'tail(3) emits the last three, oldest first';

($got, $done) = run({ .tail(10) });
is-deeply $got, (1, 2, 3), 'tail(N) of a shorter source emits everything';

($got, $done) = run({ .tail(0) });
is-deeply $got, (), 'tail(0) emits nothing';
ok $done, '... but still finishes';

($got, $done) = run({ .tail(*) });
is-deeply $got, (1, 2, 3), 'tail(*) keeps everything';

# Nothing is released before the source is done.
{
    my $s = Supplier.new;
    my @got;
    $s.Supply.tail(2).tap({ @got.push($_) });
    $s.emit($_) for 1..3;
    is-deeply @got.List, (), 'nothing is emitted while the source is still open';
    $s.done;
    is-deeply @got.List, (2, 3), '... the tail arrives at done';
}

# tail(1) of a source that emitted nothing still emits one undefined value.
{
    my $s = Supplier.new;
    my @got;
    $s.Supply.tail(1).tap({ @got.push($_) });
    $s.done;
    is @got.elems, 1, 'tail(1) of an empty source emits one value';
    ok !@got[0].defined, '... an undefined one';
}
{
    my $s = Supplier.new;
    my @got;
    my $done = False;
    $s.Supply.tail(2).tap({ @got.push($_) }, done => { $done = True });
    $s.done;
    is @got.elems, 0, 'tail(2) of an empty source emits nothing';
    ok $done, '... and finishes';
}

# A tail stage composes with the others, in both directions.
($got, $done) = run({ .map(* * 2).tail(2) }, 4);
is-deeply $got, (6, 8), 'map then tail';
($got, $done) = run({ .tail(2).map(* * 10) }, 4);
is-deeply $got, (30, 40), 'tail then map';
($got, $done) = run({ .grep(* %% 2).tail(2).reduce(&[+]) }, 8);
is-deeply $got, (14,), 'grep, tail, then reduce: the stage finishes its derived supply';

# .first(:end) is grep(|c).tail.
{
    my $s = Supplier.new;
    my @got;
    $s.Supply.first(:end, * < 3).tap({ @got.push($_) });
    $s.emit($_) for 1..3;
    $s.done;
    is-deeply @got.List, (2,), '.first(:end, ...) on a live supply emits the last match at done';
}

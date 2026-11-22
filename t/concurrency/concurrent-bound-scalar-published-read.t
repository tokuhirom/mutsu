use Test;

# ADR-8069 slice 1: once a second thread exists, a `:=`-bound scalar holding a
# payload-free value (small Int, Num, Bool, type object) is read lock-free
# through a word the cell publishes. These races check that the published word
# never goes stale: every write is visible after it is synchronized, readers
# only ever see values that were stored, and a cell whose slot was handed out
# as a raw pointer (an element store autovivifying it) stops being published.

plan 7;

# A write made on another thread is visible once awaited, even after this
# thread read (and so published) the old value.
{
    my $src = 7;
    my $x := $src;
    await start { 1 };          # a second mutator thread now exists
    is $x, 7, 'bound scalar reads its value';
    await start { $x = 42 };
    is $x, 42, 'a write on another thread is visible after await';
}

# Readers racing a writer see only stored values, never decreasing, and the
# final value once the writer is awaited.
{
    my $src = 0;
    my $x := $src;
    my $n = 20_000;
    my $writer = start { for 1..$n -> $i { $x = $i }; True };
    my @readers = (^3).map: { start {
        my $last = 0;
        my $ok = True;
        for ^20_000 {
            my $v = $x;
            $ok = False unless $v ~~ Int && $last <= $v <= $n;
            $last = $v;
        }
        $ok
    } };
    await $writer;
    ok all(await @readers), 'racing readers see only stored values, in order';
    is $x, $n, 'the last write is visible to every reader';
}

# Alternating payload-free and heap values: a reader sees either kind, intact.
{
    my $src = 0;
    my $x := $src;
    my $writer = start {
        for ^5_000 -> $i { $x = $i %% 2 ?? $i !! "s$i" };
        True
    };
    my $reader = start {
        my $ok = True;
        for ^20_000 {
            my $v = $x;
            $ok = False unless $v ~~ Int || ($v ~~ Str && $v.starts-with('s'));
        }
        $ok
    };
    await $writer;
    ok await($reader), 'readers see whole Int or Str values while they alternate';
}

# An element store autovivifies the scalar's slot through a raw pointer; the
# cell must not keep serving the value it published before.
{
    my $src;
    my $x := $src;
    await start { 1 };
    ok !$x.defined, 'unset bound scalar reads as a type object';
    await start { $x[0] = 5 };
    is-deeply $x, [5], 'autovivified element store through the bound scalar is visible';
}

use Test;

# ADR-17216 slice 1: the IO handle table is one process-wide table. A handle
# opened on any thread is usable on every other thread, whatever way the
# opener's Promise is awaited, and an operation on an inherited handle (close,
# a read that moves the position) is seen by every thread holding it. Each
# expectation below matches `raku`.

plan 8;

my $dir = $*TMPDIR.add("mutsu-shared-io-table-{$*PID}");
$dir.mkdir;
LEAVE { .unlink for $dir.dir; $dir.rmdir }

# Sibling workers open handles at the same time. Their handles must stay
# distinct: before the shared table, both took the same id from the parent's
# copied counter and the merge on `await` dropped the second.
{
    my $p1 = start { open $dir.add('a.txt'), :w };
    my $p2 = start { open $dir.add('b.txt'), :w };
    my ($fa, $fb) = await $p1, $p2;
    $fa.print('alpha');
    $fb.print('beta');
    $fa.close;
    $fb.close;
    is $dir.add('a.txt').slurp, 'alpha', 'first sibling handle writes its own file';
    is $dir.add('b.txt').slurp, 'beta', 'second sibling handle writes its own file';
}

# `await` of an array of promises: the handles opened by every worker are
# usable afterwards (the old transfer only ran for a single-Promise await).
{
    my @p = (^4).map: -> $i { start { open $dir.add("arr-$i.txt"), :w } };
    my @h = await @p;
    for @h.kv -> $i, $h { $h.print("n$i"); $h.close }
    is (^4).map({ $dir.add("arr-$_.txt").slurp }).join(','), 'n0,n1,n2,n3',
        'handles opened by an array of awaited promises are all usable';
}

# A handle opened on a worker and reached through `.result`.
{
    my $p = start { open $dir.add('result.txt'), :w };
    my $h = $p.result;
    $h.say('via result');
    $h.close;
    is $dir.add('result.txt').slurp, "via result\n", 'handle reached through .result is usable';
}

# A close on a worker is seen by the thread that opened the handle.
{
    my $h = open $dir.add('closed.txt'), :w;
    await start { $h.close };
    nok $h.opened, 'a close on a worker closes the handle for its opener';
}

# A read on a worker moves the position every thread sees.
{
    $dir.add('lines.txt').spurt("one\ntwo\nthree\n");
    my $h = open $dir.add('lines.txt');
    my $first = await start { $h.get };
    is $first, 'one', 'worker reads the first line';
    is $h.get, 'two', 'the opener continues where the worker stopped';
    $h.close;
}

# A handle opened on a worker that is never returned is still the worker's
# own, live handle when another thread reaches it through a shared container.
{
    my @shared;
    my $l = Lock.new;
    await (^3).map: -> $i {
        start {
            my $h = open $dir.add("shared-$i.txt"), :w;
            $l.protect: { @shared.push: $h };
        }
    };
    .print('x') for @shared;
    .close for @shared;
    is (^3).map({ $dir.add("shared-$_.txt").slurp }).join, 'xxx',
        'handles published through a shared array by workers are all usable';
}

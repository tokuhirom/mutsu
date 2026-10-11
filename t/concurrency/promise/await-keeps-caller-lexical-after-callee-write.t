use v6;
use Test;

# #11545: a routine whose closure writes its own `my $now` and then starts a
# thread published that write into the cross-thread shared store, which is
# keyed by bare name. The caller's `await` then pulled it back into the
# CALLER's unrelated `my $now`. A lineage's own writes are already in its env,
# so an await must not pull them back.

plan 4;

{
    my $lock = Lock.new;
    sub get() {
        my $now = Nil;
        $lock.protect({
            $now = 7;
            start { };
        });
        Promise.kept(1)
    }
    my @seen;
    for 1..3 {
        my $now = "main$_";
        await get();
        @seen.push: $now;
    }
    is-deeply @seen, ["main1", "main2", "main3"],
        'a callee closure write to its own $now does not reach the caller across await';
}

{
    class C {
        has $!lock = Lock.new;
        method get() {
            my $now = Nil;
            $!lock.protect({
                $now = now;
                my $p = Promise.start({ 1 });
            });
            Promise.kept(1)
        }
    }
    my $c = C.new;
    my @seen;
    for 1..3 {
        my $now = "main$_";
        await $c.get;
        @seen.push: $now;
    }
    is-deeply @seen, ["main1", "main2", "main3"], 'same through a method';
}

{
    # A worker's write to a variable it shares with the awaiting thread must
    # still arrive.
    my $x = 1;
    await start { $x = 2 };
    is $x, 2, 'a worker write is still visible after await';
    my $y = 0;
    await (^4).map: -> $i { start { $y = 10 } };
    is $y, 10, 'and from several workers';
}

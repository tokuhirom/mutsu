use v6;
use Test;

# Same-thread visibility of shared-hash element writes: a `%h{$k} = $v`
# performed inside a `start` block lands in the __mutsu_atomic_hash:: shared
# store, and the writing thread's own re-read of %h must observe it
# (the base-key snapshot in shared_vars is stale). Behavior verified
# against raku.

plan 12;

{
    my %h;
    my $inside;
    my $inside-elem;
    my $inside-exists;
    await start {
        %h<k> = [1, 2, 3];
        $inside = %h.elems;
        $inside-elem = %h<k>[1];
        $inside-exists = %h<k>:exists;
    };
    is $inside, 1, 'writing thread sees its own hash element write via %h';
    is $inside-elem, 2, 'writing thread reads the stored element back';
    ok $inside-exists, ':exists sees the element inside the thread';
    is %h.elems, 1, 'parent sees the write after await';
    is-deeply %h<k>, $[1, 2, 3], 'parent reads the stored value';
}

{
    my %m;
    my $keys;
    await start {
        %m<p> = 1;
        %m<q> = 2;
        $keys = %m.keys.sort.join(',');
    };
    is $keys, 'p,q', 'keys/iteration inside the thread sees both writes';
    is %m.elems, 2, 'parent sees both writes';
}

{
    my %c;
    await (^10).map: -> $i { start { %c{"k$i"} = $i } };
    is %c.elems, 10, 'concurrent element writes from sibling threads all land';
}

{
    my @fresh;
    for ^2 {
        my %f;
        await start { %f<x> = $_; };
        @fresh.push(%f.elems, %f<x>);
    }
    is-deeply @fresh, [1, 0, 1, 1], 'loop re-declaration gets a fresh hash each iteration';
}

{
    my %n;
    my $nested;
    await start {
        %n<a> = {};
        %n<a><b> = 5;
        $nested = %n<a><b>;
    };
    is $nested, 5, 'nested element write is visible inside the thread';
    is %n<a><b>, 5, 'nested element write is visible to the parent';
}

{
    my %d;
    my $after-delete;
    await start {
        %d<x> = 1;
        %d<x>:delete;
        $after-delete = %d.elems;
    };
    is $after-delete, 0, ':delete after a shared element write is visible in-thread';
}

use v6;
use Test;

# A `start` inside a method used to seed the method's attribute aliases
# (`@!x`, `$!lock`) into the thread-shared variable store. An inline
# `$!lock.protect: { @!x ... }` later in the same method then read that
# spawn-time snapshot and lost every write another thread had made to the
# attribute since (#8380: `Test::Scheduler.run-due` dropped a `FutureEvent`
# pushed by the main thread).

plan 4;

class Box {
    has @!items;
    has $!lock = Lock.new;
    method add($x) { $!lock.protect: { @!items.push: $x } }
    method observe($ready, $go) {
        $!lock.protect: { @!items = () };
        start { 1 };
        $ready.keep;
        await $go;
        my $outside = @!items.elems;
        my $inside = $!lock.protect: { @!items.elems };
        ($outside, $inside)
    }
}

{
    my $b = Box.new;
    my $ready = Promise.new;
    my $go = Promise.new;
    my $t = Promise.start: { $b.observe($ready, $go) };
    await $ready;
    $b.add(7);
    $go.keep;
    my ($outside, $inside) = await $t;
    is $outside, 1, 'a plain attribute read sees the other thread\'s push';
    is $inside, 1, 'an inline protect block sees it too';
}

class Counter {
    has @!seen;
    has $!n = 0;
    method run() {
        await start { @!seen.push: 1; $!n++ };
        (@!seen.elems, $!n)
    }
}

{
    my ($elems, $n) = Counter.new.run;
    is $elems, 1, 'an attribute written inside the start block is visible after await';
    is $n, 1, 'a scalar attribute too';
}

# vim: expandtab shiftwidth=4

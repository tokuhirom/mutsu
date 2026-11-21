use Test;

# A removal (`shift`, `pop`, `splice`) on a private aggregate attribute inside
# a `start` block acts on the instance's own array -- the one other threads
# pushed into after the block was spawned -- not on a spawn-time snapshot of
# it. The attribute is addressed through its slot (ADR-18356 D2); the
# name-keyed thread-shared lane answered the snapshot, so the Mux
# distribution's controller died with "Cannot shift from an empty Array" and
# its drain test hung. Each case is checked against rakudo.

plan 8;

# The Mux shape: the worker is spawned in BUILD, the queue is filled later.
{
    my class Q {
        has @!queue;
        has $!fed;
        has $!worker;
        has @.seen;
        submethod BUILD(:@!queue = []) {
            $!fed = Promise.new;
            $!worker = start {
                await $!fed;
                while @!queue.elems {
                    @!seen.push: @!queue.shift;
                }
            };
        }
        method feed(*@items) { @!queue.push(|@items); $!fed.keep }
        method wait { await $!worker }
        method left { @!queue.elems }
    }
    my $q = Q.new;
    $q.feed(1, 2, 3);
    $q.wait;
    is-deeply $q.seen, [1, 2, 3], 'shift in a spawned block takes the items pushed after the spawn';
    is $q.left, 0, 'and empties the instance queue';
}

# pop and splice, from a method's own start block.
{
    my class P {
        has @!items;
        method run(&fill) {
            my $go = Promise.new;
            my $t = start { await $go; (@!items.pop, @!items.splice(0, 1)) };
            fill(self);
            $go.keep;
            await $t
        }
        method add(*@x) { @!items.push(|@x) }
        method items { @!items }
    }
    my $p = P.new;
    my ($popped, $spliced) = $p.run(-> $o { $o.add(<a b c>) });
    is $popped, 'c', 'pop in a spawned block sees later pushes';
    is-deeply $spliced, ['a'], 'splice in a spawned block sees later pushes';
    is-deeply $p.items, ['b'], 'and the instance keeps what is left';
}

# A hash attribute's removal in a spawned block.
{
    my class H {
        has %!h;
        method run {
            my $go = Promise.new;
            my $t = start { await $go; %!h<x>:delete };
            %!h<x> = 42;
            $go.keep;
            await $t
        }
        method keys { %!h.keys }
    }
    my $h = H.new;
    is $h.run, 42, 'delete in a spawned block sees a later store';
    is $h.keys.elems, 0, 'and removes it from the instance';
}

# The main thread still sees the worker's removals.
{
    my class W {
        has @!q = 1, 2;
        method drain { await start { @!q.shift }; @!q }
    }
    is-deeply W.new.drain, [2], 'a removal in a worker is seen by the spawning method';
}

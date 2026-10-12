use Test;

# #12484: once a second mutator thread had been spawned, `.map`/`.grep` on an
# Array ran on a snapshot taken under the container stripe (ADR-0068 §16), so a
# callback or a `for` loop writing through `$_` updated the snapshot and the
# live array never changed. `.map`/`.grep` on an Array now read the live array
# under the stripe and write back into it (ADR-0068 §16.1).

plan 7;

# Any earlier `start` switches the process into multi-mutator mode.
await start { 1 };

{
    my @a = 1, 2, 3;
    for @a.grep(* > 2) { $_ = 0 }
    is-deeply @a, [1, 2, 0], 'a for loop over .grep writes into the array after a thread ran';
}

{
    my @a = 1, 2, 3;
    @a.grep(* > 1)>>++;
    is-deeply @a, [1, 3, 4], '>>++ over .grep writes into the array after a thread ran';
}

{
    my @a = 1, 2, 3;
    @a.list.map({ $_++ }).eager;
    is-deeply @a, [2, 3, 4], '.map over @a.list writes into the array after a thread ran';
}

class Holder {
    has @!a = 1, 2, 3;
    method bump-map  { @!a.map({ $_ *= 10 }).eager; @!a }
    method zero-grep { for @!a.grep(* > 20) { $_ = 0 }; @!a }
}

{
    my $h = Holder.new;
    is-deeply $h.bump-map, [10, 20, 30], '.map writes into an attribute array after a thread ran';
    is-deeply $h.zero-grep, [10, 20, 0], '.grep writes into an attribute array after a thread ran';
}

# The live array is read and written under its stripe while other threads push
# to it. This pins mutsu's memory-safety guarantee, not a Raku answer.
class Shared {
    has @!a;
    method go {
        @!a.push(1);
        for @!a.grep(* == 1).head(2) { $_ = 1 }
        @!a.map({ $_ }).elems;
    }
}

{
    my $s = Shared.new;
    my $max = 0;
    await (^3).map: { start { my $end = now + 0.6; while now < $end { $max max= $s.go } } };
    ok $max > 0, '.map/.grep on an array three threads push to does not crash';
}

{
    my %h = a => 1, b => 2;
    is %h.map({ .key }).sort.join(','), 'a,b', '.map on a hash still runs after a thread ran';
}

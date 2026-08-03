use v6;
use Test;

plan 5;

# A `state` variable declared inside a loop body accumulates across the
# iterations of one execution of that loop statement.
#
# The loop-body state sync used to read the variable's value back from the
# name-keyed env. That was only correct because a frame containing a for-loop
# had every local env-synced by a frame-wide blanket. ADR-0018 narrowed that to
# the loop's own baked slots, at which point a plain `state $t` no longer
# mirrored to env and the sync persisted the value the declaration seeded
# instead of the value the body wrote -- so the accumulation vanished and every
# iteration saw the initializer again.

{
    my @g = gather { for 1 .. 3 { state $t = 0; $t = $t + 1; take $t; } };
    is @g.join(" "), "1 2 3", "state in a gather-wrapped for body accumulates";
}

{
    my @g = gather for 1 .. 3 { state $u = 0; $u = $u + 1; take $u; };
    is @g.join(" "), "1 2 3", "state in a `gather for` statement body accumulates";
}

{
    my @seen;
    for 1 .. 3 { state $s = 0; $s = $s + 1; @seen.push($s); }
    is @seen.join(" "), "1 2 3", "state in a plain for body accumulates";
}

{
    my @seen;
    my $i = 0;
    while $i++ < 3 { state $w = 0; $w = $w + 1; @seen.push($w); }
    is @seen.join(" "), "1 2 3", "state in a while body accumulates";
}

# The uniq example from S04-control.pod (roast/S04-statements/gather.t): the
# `state` holds the previous element, so a lost write makes every duplicate
# get taken again.
{
    my @list = 1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 6, 6;
    my @uniq = gather for @list {
        state $previous = take $_;
        next if $_ === $previous;
        $previous = take $_;
    }
    is @uniq.join(" "), "1 2 3 4 5 6", "state carries the previous element across iterations";
}

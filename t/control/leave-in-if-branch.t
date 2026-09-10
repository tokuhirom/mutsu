use Test;

# LEAVE (and friends) inside an `if`/`else` branch fire when the branch
# exits — in both statement position and value position (a sub's trailing
# `if`). OO::Monitors unlocks its per-instance lock in a LEAVE inside
# `if SELF.DEFINITE { ... }`; before this fix the phaser never ran there,
# so the lock stayed held and any cross-thread call deadlocked.

plan 6;

my @events;

sub value-if($flag) {
    if $flag {
        @events.push('enter-then');
        LEAVE @events.push('leave-then');
        42
    }
    else {
        @events.push('enter-else');
        LEAVE @events.push('leave-else');
        0
    }
}

is value-if(True), 42, 'value-position if branch still delivers its value';
is-deeply @events, [<enter-then leave-then>],
    'LEAVE in the then-branch fired at branch exit (value position)';

@events = ();
is value-if(False), 0, 'value-position else branch still delivers its value';
is-deeply @events, [<enter-else leave-else>],
    'LEAVE in the else-branch fired at branch exit (value position)';

@events = ();
sub stmt-if($flag) {
    if $flag {
        LEAVE @events.push('leave-stmt');
        @events.push('body');
    }
    return 'done';
}
is stmt-if(True), 'done', 'statement-position if still runs';
is-deeply @events, [<body leave-stmt>],
    'LEAVE in a statement-position if branch fired at branch exit';

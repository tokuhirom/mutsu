use Test;

# A routine-local `my subset` is lexical to its routine: the `EVAL` of a plain
# sub that routine CALLS must not resolve it (#16099). The routine's own
# `EVAL`, a closure a helper runs, later uses in the routine and a mainline
# block's subset are unaffected.

plan 7;

sub callee { (try EVAL q[my Small $x = 5; $x]) // 'nv-subset' }

sub caller-of-callee { my subset Small of Int where * < 10; callee() }
is caller-of-callee(), 'nv-subset', "the callee's EVAL does not see the caller's my subset";

sub own-eval { my subset Small of Int where * < 10; EVAL q[my Small $x = 5; $x] }
is own-eval(), 5, "the declaring routine's own EVAL sees it";

sub via-helper {
    my subset Small of Int where * < 10;
    my $r;
    lives-ok { $r = EVAL q[my Small $x = 5; $x] }, "a closure run by a helper sub sees it";
    $r
}
is via-helper(), 5, '... and answers the value';

sub still-usable { my subset Small of Int where * < 10; callee(); my Small $y = 3; $y }
is still-usable(), 3, 'the subset is back for the rest of the routine after the callee EVAL';

sub constrained { my subset Small of Int where * < 10; callee(); my Small $y = 3; try { $y = 50 }; $y }
is constrained(), 3, '... and still constrains';

{
    my subset Blk of Int where * < 10;
    sub in-block { (try EVAL q[my Blk $x = 5; $x]) // 'nv' }
    is in-block(), 5, 'a sub declared beside a mainline-block subset still sees it';
}

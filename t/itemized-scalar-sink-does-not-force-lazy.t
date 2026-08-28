use v6;
use Test;

# A plain scalar assignment (`$s = SEQ` / `my $s = SEQ`) itemizes a lazy
# Seq/gather into a Scalar container. Per raku, discarding an ITEMIZED value
# in sink context is a no-op (only a warning, "Useless use of ... in sink
# context") -- it does NOT run the lazy source for side effects. Only a
# genuinely un-itemized (bare) Seq/gather forces on sink.
#
# mutsu's sink-forcing (`OpCode::SinkPop`, `OpCode::SinkPopAssign`, and the
# statement-level-call sink path in `vm_call_exec_ops.rs`) forced ANY lazy
# value discarded in sink context, regardless of whether it had been
# assigned to a scalar first -- so `my $s = (gather die)[]` reified the
# gather (running `die`) the moment `$s` was later read in sink context, or
# the moment the value flowed back out through a routine/closure return
# whose caller discarded it. raku never reifies in either case.

plan 8;

# The exact roast/S02-types/array.t assertion this fix targets: a zen slice
# on a lazy gather does not reify, even under `lives-ok`/a plain sub call
# that discards the return value.
{
    sub call-it(&c) { my $ok = 1; c(); CATCH { default { $ok = 0 } }; $ok }
    is call-it({ my $s = (gather die)[] }), 1,
        'a zen-sliced gather assigned to a scalar does not reify through a discarded closure return';
}

# Without the scalar assignment, the same shape DOES still reify (raku
# agrees) -- pinning that the fix is itemization-specific, not "closures
# never force".
{
    sub call-it2(&c) { c(); 1 }
    my $died = False;
    try { call-it2({ (gather die)[] }) };
    $died = so $!;
    ok $died, 'a bare (unassigned) zen-sliced gather still reifies through a discarded closure return';
}

# The minimal top-level case: sinking a bare scalar that holds an
# unreified gather must not force it.
{
    my $lived = True;
    my $s = (gather die)[];
    try { $s; CATCH { default { $lived = False } } }
    ok $lived, 'sinking a bare itemized scalar does not force its gather';
}

# Same, but without the zen slice at all -- itemization alone (no `[]`)
# must suppress the forcing.
{
    my $lived = True;
    my $s = gather { die };
    try { $s; CATCH { default { $lived = False } } }
    ok $lived, 'sinking a bare itemized scalar (no zen slice) does not force its gather';
}

# Control: an ordinary discarded statement-level `.map`/`.grep` sink still
# forces side effects as before (unrelated to itemization) -- regression
# guard that the fix did not disable sink-forcing in general.
{
    my @seen;
    (1..3).map({ @seen.push($_) });
    is @seen.elems, 3, 'a bare discarded .map still forces (unrelated to itemization)';
}

# Control: consuming the itemized scalar's value later still reifies (and
# still dies) -- itemization defers forcing, it does not cancel it.
{
    my $died = False;
    my $r = do { my $s = (gather die)[] };
    try { $r.raku };
    $died = so $!;
    ok $died, 'later consuming an itemized scalar still forces its gather';
}

# Control: never touching the itemized scalar at all keeps it lazy forever.
{
    my $lived = True;
    try {
        my $r = do { my $s = (gather die)[] };
        1;
        CATCH { default { $lived = False } }
    }
    ok $lived, 'an itemized scalar that is never consumed stays lazy';
}

# Case from the ticket: the same shape nested inside a bare block (not a
# routine call) with its own CATCH.
{
    my $caught;
    {
        my $s = (gather die)[];
        $s;
        CATCH { default { $caught = $_.^name } }
    }
    ok !$caught.defined, 'sinking an itemized scalar inside a bare block CATCH stays uncaught (nothing thrown)';
}

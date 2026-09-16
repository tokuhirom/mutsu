use Test;

# A positional element store into a BARE `LazyList` (a `gather`, a lazy
# pipe, a finite `.lazy` view, ... held by a `$`/list-context variable, NOT
# an `@`-array-context one) is decided per element, exactly like a `Seq`
# (see t/collections/transform/producer-seq-named-receiver-write.t). mutsu
# had no arm for a bare LazyList target at all: the generic named-element-
# assign tail's final autovivify fallback silently REPLACED the whole
# LazyList with a fresh one-element Array, dropping every element past the
# touched index.
#
# my $s = gather { take 1; take 2 }; $s[0] = 5; say $s;
#   raku:  X::Assignment::RO, "Cannot modify an immutable Int (1)"
#   mutsu (before): silently succeeds AND TRUNCATES -- prints `[5]`, not `[5 2]`
#
# `reify_lazy_array_slot` already handles the LEGITIMATE array-context case
# (`my @a = 1,2,4...Inf; @a[2] = 99` really does write through) by
# materializing the touched prefix before this op runs; `LazyList::
# in_array_context()` is what tells the two apart, so refusing every bare
# LazyList element cannot regress that case.

plan 11;

throws-like
    { my $s = (gather { take 1; take 2 }); $s[0] = 5; },
    X::Assignment::RO,
    'storing into a bare gather LazyList element refuses (does not truncate)';

{
    my $s = (gather { take 1; take 2 });
    my $threw = False;
    try { $s[0] = 5; CATCH { default { $threw = True; } } }
    ok $threw, 'the store above actually threw (did not silently succeed)';
    # A bounded index read (not `.eager`) here deliberately: forcing the
    # refusal probe already pulled ONE element through the bounded
    # (coroutine-resumable) force path, and reading the next element the
    # same way is what confirms the source can still continue -- exercising
    # `.eager`/a strict force here would hit a separate, pre-existing gap
    # where the STRICT force path short-circuits on any non-empty cache
    # instead of checking whether the coroutine that produced it is
    # actually finished (see the ticket this pins).
    is $s[1], 2, 'the source can still continue past the refused element';
}

# A finite `.lazy` view behaves the same way.
throws-like
    { my $s = (1, 2, 3).lazy; $s[0] = 9; },
    X::Assignment::RO,
    'storing into a bare .lazy element refuses';

# Past the end: rakudo's AT-POS hands back Nil and refuses the store on it.
throws-like
    { my $s = (gather { take 1; take 2 }); $s[5] = 9; },
    X::Assignment::RO,
    'storing past the end of a bare gather LazyList refuses (Nil element)';

# The legitimate array-context case is unaffected: an infinite sequence
# assigned to an @-array still writes through its touched element.
{
    my @a = 1, 2, 4 ... Inf;
    @a[2] = 99;
    is @a[0..3], (1, 2, 99, 8), 'array-context lazy sequence element store still writes through';
}

# take-rw preserves a real container even in a bare (list-context) gather,
# and that element must still write through.
{
    my @spot = 10, 20, 30;
    my $s = gather { take-rw @spot[1] };
    $s[0] = 999;
    is @spot, (10, 999, 30), 'take-rw element in a bare gather writes through the shared cell';
    is $s[0], 999, 'the bare gather itself observes the write-through too';
}

# A bare (non-array-context) infinite arithmetic sequence held in a $-scalar
# refuses the same way a gather does -- it is not special-cased to gathers.
throws-like
    { my $s = (1, 2, 4 ... Inf); $s[2] = 99; },
    X::Assignment::RO,
    'a bare infinite sequence held in a scalar refuses its element store too';

# roast/S04-statements/gather.t (old-issue-tracker #4668): `take-rw` of a
# non-variable lvalue -- here an inline scalar declaration -- currently
# loses its container identity when pulled through this lazy (coroutine)
# path (unlike `take-rw @spot[1]` above, which arrives as a genuine
# ContainerRef and writes through correctly). mutsu cannot currently tell
# that shape apart, at the value level, from a plain `take` of an
# explicitly-itemized value (which raku DOES refuse -- see the throws-like
# below), so this element-store fix deliberately DECLINES on it rather
# than guessing wrong in either direction, falling through to the
# pre-existing (also imperfect) behavior. Tracked as
# https://github.com/tokuhirom/mutsu/issues/8521; this pins the *current*
# (not fully correct, but not regressed) mutsu behavior so a future fix
# for #8521 is guided by a green test turning into a stricter one, not by
# a silent behavior change.
lives-ok
    { my $l = gather { take-rw my $ = 1 }; $l.AT-POS(0) = 42; },
    'take-rw of an inline scalar declaration in a bare gather does not throw (matches roast/S04-statements/gather.t; see #8521)';

# A plain (non-rw) take of an explicitly-itemized value is NOT the same
# shape rakudo writes through -- confirms the decline above does not
# accidentally widen to accept this too. `dies-ok` rather than
# `throws-like`: this falls through to the pre-existing autoviv-replace
# path (unchanged by this fix either way), which already refused with a
# different exception class/wording than raku's `X::AdHoc` before this fix
# existed -- out of scope here.
dies-ok
    { my $s = gather { take $(1, 2, 3) }; $s[0] = 99; },
    'a plain take of an explicitly-itemized value still refuses';

# vim: expandtab shiftwidth=4

use v6;
use Test;

# A matching `when` inside a `.THREAD` block leaves *that block* with `succeed`.
# `.THREAD`'s loop propagated the signal instead of absorbing it, so the
# remaining eigenstates were skipped and the enclosing routine unwound with the
# `when` body's value. Every other loop construct already absorbs it (see the
# `for` body in vm_for_loop_body.rs), and `for` over the same values was correct
# throughout, which is why this stayed hidden.
#
# Found via roast's Test::Util `is-deeply-junction`, whose `junction-guts`
# helper is written exactly this way: it gutted only the first eigenstate of
# `any(all(1,2), 3)`.

plan 6;

sub collect(Junction $j) {
    my @seen;
    $j.THREAD: {
        when Junction { @seen.push: 'J' }
        @seen.push: $_;
    }
    @seen;
}

is-deeply collect(any(all(1, 2), 3)), ['J', 3],
    'a matching `when` does not abort the rest of the THREAD loop';
is-deeply collect(any(3, all(1, 2))), [3, 'J'],
    'and not when the matching eigenstate comes last either';
is-deeply collect(any(all(1, 2), all(3, 4))), ['J', 'J'],
    'every eigenstate is visited when they all match';
is-deeply collect(any(1, 2, 3)), [1, 2, 3],
    'a block whose `when` never matches is unchanged';

# The `when` body calling a routine was the shape that first showed the bug:
# the leaked signal unwound the *caller* too, so `guts` returned early.
sub guts(Junction $j) {
    $j.gist ~~ /^ $<type>=(\w+)/;
    my $type := ~$<type>;
    my @g;
    $j.THREAD: {
        when Junction { @g.push: guts $_ }
        @g.push: $_;
    }
    [$type, @g.sort.List]
}

is-deeply guts(any(all(1, 2), 3)), ['any', (3, ['all', (1, 2)])],
    'a recursive gutting of a nested junction sees both eigenstates';
is-deeply guts(all('a', 'b')), ['all', ('a', 'b')],
    'and a flat junction still guts correctly';

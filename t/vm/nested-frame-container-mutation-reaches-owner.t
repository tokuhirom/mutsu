use Test;

# ADR-0039 §6 acceptance row (a): a container mutated from a NESTED frame must
# reach its owner's binding, and must not reach a same-named binding that
# happens to be live in whatever scope the mutation was triggered from.
#
# Before ADR-0055 slice 1b (`da8e94252`, "an escaping container capture the
# frame cannot vouch for gets a cell") the write propagated by NAME: the
# mutating frame has no slot for `@a`, so the write landed in `env`, where the
# inner block's own `my @a` was sitting. Every row below answered the inner
# block's container instead of the closure's own, and the owner's binding was
# left stale -- and when the write path REPLACED the container rather than
# mutating it in place, the owner's slot went stale even with no shadow in
# sight.
#
# This was the last blocker ADR-0039 slice 2 (the `@`/`%` read flip) named for
# itself, so it is pinned here rather than left resting on the ADR's prose.
# Every expectation was measured against raku v2026.07.

plan 7;

# In-place growth, with a same-named shadow live at the call site.
sub grow {
    my @a = 1, 2;
    my $push = sub { @a.push(9) };
    my $inner;
    { my @a = 3; $push(); $inner = @a }
    ($inner, @a)
}
my ($inner, @outer) = grow();
is $inner, [3], 'the shadowing block keeps its own container';
is @outer, [1, 2, 9], 'and the closure mutates the container it closed over';

# Whole-container REPLACEMENT from the nested frame. This is the half that
# stayed broken even without a shadow: the owner's slot went stale because the
# write installed a fresh container under the name.
sub replace {
    my @a = 1, 2;
    my $s = sub { @a = 7, 8 };
    { my @a = 3; $s() }
    @a
}
is replace(), [7, 8], 'a replacing write from a nested frame reaches the owner';

# Shrinking in place (`.shift`), the shape `roast/S15-nfg/concat-stable.t` uses.
sub shrink {
    my @o = 1, 2, 3;
    my $s = sub { @o.shift };
    { my @o = 9; $s() }
    @o
}
is shrink(), [2, 3], 'a shrinking write from a nested frame reaches the owner';

# The hash twin, both halves.
sub hash-grow {
    my %h = a => 1;
    my $s = sub { %h<b> = 2 };
    { my %h = c => 9; $s() }
    %h.keys.sort
}
is hash-grow(), ('a', 'b'), 'a key added from a nested frame reaches the owner hash';

sub hash-replace {
    my %h = a => 1;
    my $s = sub { %h = (z => 9) };
    { my %h = c => 9; $s() }
    %h.keys.sort
}
is hash-replace(), ('z',), 'a replacing hash write from a nested frame reaches the owner';

# `.=` rebuilds the container and stores it back; same hazard as `replace`.
sub rebuild {
    my @a = 3, 1, 2;
    my $s = sub { @a .= sort };
    { my @a = 9; $s() }
    @a
}
is rebuild(), [1, 2, 3], 'a `.=` rebuild from a nested frame reaches the owner';

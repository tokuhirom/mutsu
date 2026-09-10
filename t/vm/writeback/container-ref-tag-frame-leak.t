use v6;
use Test;

# A TagContainerRef signal set inside a callee frame must not leak into the
# caller's next for/given: the tag carries a compile-time local SLOT number
# that only means something in the frame that set it. Before the fix, the
# method below leaves an unconsumed tag ("@src", slot-in-method-frame) behind
# (a do-block whose tail is a bare @-var tags it for potential container
# writeback, but nothing consumes it), and the caller's `for gen() -> $y`
# loop — whose source is a sub call, so it emits no tag of its own — adopted
# the stale tag and wrote its loop items back over whatever CALLER local
# happened to live at that slot index (Text::CSV t/90_csv.t 507-508).

class Tagger {
    method churn() {
        my @src = 1, 2;
        # An expression-position assignment (the RHS of `or`) tags @src for
        # potential container-writeback consumers; nothing here consumes it.
        @src.elems == 99 or @src = 3, 4;
        @src.elems;
    }
}

sub gen() { [1, 2, 3] }

# Same-named caller variable plus array canaries: the stale tag's writeback
# resolved the name in the CALLER's env and its baked slot in the CALLER's
# frame, so the caller array living at the method-frame slot index (one of
# the canaries below) had its elements overwritten with the loop's items.
my @src = <x y z>;
my @c0 = <a0 b0 c0>;
my @c1 = <a1 b1 c1>;
my @c2 = <a2 b2 c2>;
my @c3 = <a3 b3 c3>;
my @c4 = <a4 b4 c4>;

Tagger.churn;

my @collected;
for gen() { @collected.push($_) }

is-deeply @collected, [1, 2, 3], "loop over sub-call source iterates normally";
is-deeply @src, [<x y z>], "caller's same-named array untouched by the loop";
is-deeply @c0, [<a0 b0 c0>], "caller array at slot 0 untouched";
is-deeply @c1, [<a1 b1 c1>], "caller array at slot 1 untouched";
is-deeply @c2, [<a2 b2 c2>], "caller array at slot 2 untouched";
is-deeply @c3, [<a3 b3 c3>], "caller array at slot 3 untouched";
is-deeply @c4, [<a4 b4 c4>], "caller array at slot 4 untouched";

done-testing;

use v6;
use Test;

# A Proxy returned from an `is rw` AT-POS must (1) run its STORE when the
# element is assigned through subscript syntax, and (2) keep its deferred
# FETCH/STORE closures bound to the method's own lexical even when a
# same-named lexical exists in the enclosing (mainline) scope.
# Both defects were found while verifying ADR-0061 and are name-independent:
# the collision is on an ordinary name ($slf), not on `self`.

plan 6;

my $slf = 1;   # the shadowing outer lexical for the second half

class B {
    has @.nodes;
    method AT-POS($offset) is rw {
        my $slf = self;
        Proxy.new(
            FETCH => method () { $slf.nodes[$offset] },
            STORE => method ($val) { $slf.nodes[$offset] = $val }
        )
    }
}

my $b = B.new(nodes => ['x', 'y']);
is $b[1], 'y', 'FETCH through the Proxy reads the element';
is $b[0], 'x', 'FETCH of the first element';

$b[0] = 'z';
is $b[0], 'z', 'STORE through the Proxy persists (read back via AT-POS)';
is $b.nodes[0], 'z', 'STORE reached the underlying attribute';

$b[1] = 'w';
is $b.nodes.join(','), 'z,w', 'a second STORE lands in the right slot';

is $slf, 1, 'the mainline lexical of the same name is untouched';

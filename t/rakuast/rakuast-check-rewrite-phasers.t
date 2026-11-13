use Test;
use experimental :rakuast;
use nqp;

# A CHECK phaser that rewrites `$*CU` makes the unit run the rewritten tree
# (#17501). The unit's own LEAVE and END phasers then run once, from that tree,
# with the values its mainline set; the program the tree replaced leaves them.
# A unit-level `LEAVE $schema.drop` in Red's t/112 used to run a second time on
# an unset `$schema`.

plan 7;

CHECK {
    sub walk($n) {
        if $n.^name eq 'RakuAST::ApplyPostfix'
            && $n.postfix.^name eq 'RakuAST::Call::Method'
            && $n.postfix.name.canonicalize eq 'flip' {
            $n.set-operands(nqp::list(RakuAST::StrLiteral.new('xyz')));
        }
        $n.visit-children(&walk);
    }
    walk($*CU);
}

class A { }
multi foo(A $a) { 'a' }
multi foo(Int $a) { 'int' }
multi prefix:<!>(A $a) { 'user !' }

my $set = 'set';
LEAVE is $set, 'set', 'the LEAVE phaser runs once, with the mainline value';
END pass 'the END phaser runs once';

is 'abc'.flip, 'zyx', 'the rewritten operand runs';
is 'b' x 3 ~ 'a', 'bbba', 'the rest of the unit runs as written';

# The tree declares the unit's multi candidates again: each replaces its
# first copy instead of making every call ambiguous.
is foo(A.new), 'a', 'a multi candidate of the unit dispatches once';
is foo(1), 'int', 'and so does its sibling';
is !A.new, 'user !', 'a user candidate of a core prefix still applies';

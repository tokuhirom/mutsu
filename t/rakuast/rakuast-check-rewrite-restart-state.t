use Test;
use experimental :rakuast;
use nqp;

# A CHECK phaser that rewrites `$*CU` makes the unit continue from the rewritten
# tree (#17501). That restart must keep the first compile's compile-time state,
# not re-run it (#17676, #17681; came from Red's t/106, whose CHECK runs
# `red-compile` and, on a second run over the rewritten tree, could not find a
# `compiled` event for its own rewritten callbacks):
#   - a statement-form BEGIN or CHECK phaser body runs once, not again over the
#     tree it rewrote;
#   - a top-level `my` declaration without an initializer keeps what its
#     BEGIN/CHECK phasers wrote into it (it adopts the first compile's binding),
#     while one with an initializer still runs the initializer.

plan 7;

# The rewrite: `'abc'.flip` becomes `'xyz'.flip` everywhere in the tree.
CHECK {
    %*ENV<MUTSU_T_RESTART_CHECK>++;
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

# A CHECK phaser writes into a top-level `my` without an initializer.
my @seen;
CHECK { @seen.push('checked') }

# A BEGIN writes into a top-level `my` without an initializer, and a
# statement-form BEGIN has a visible side effect.
my $from-begin;
BEGIN { $from-begin = 'begun'; %*ENV<MUTSU_T_RESTART_BEGIN>++ }

# A declaration WITH an initializer still runs the initializer at run time.
my $with-init = 'init-value';

is 'abc'.flip, 'zyx', 'the rewritten operand runs';
is @seen, ['checked'], 'a CHECK-written my (no initializer) keeps its value on the restart';
is $from-begin, 'begun', 'a BEGIN-written my (no initializer) keeps its value on the restart';
is $with-init, 'init-value', 'a my with an initializer still runs its initializer';
is %*ENV<MUTSU_T_RESTART_CHECK>, 1, 'the CHECK phaser runs once, not again over the rewritten tree';
is %*ENV<MUTSU_T_RESTART_BEGIN>, 1, 'the BEGIN phaser runs once, not again over the rewritten tree';
is 'a' x 2 ~ 'b', 'aab', 'the rest of the unit runs as written';

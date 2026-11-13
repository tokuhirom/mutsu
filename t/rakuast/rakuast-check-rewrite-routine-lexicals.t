use Test;
use experimental :rakuast;
use nqp;

# A unit a CHECK phaser rewrote runs its routines over the restarted
# mainline: a sub's write to a mainline lexical is seen by the mainline, also
# when the sub is called through a method or a `map` (Red t/124, #17644).

plan 3;

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

my $effects = 0;
sub tick { $effects++; 1 }
class Runner { method run(&g) { g() } }

is 'abc'.flip, 'zyx', 'the rewritten tree runs';
({ tick() },).map({ $_() }).eager;
Runner.run({ tick() });
is $effects, 2, 'calls through map and a method write the mainline lexical';
tick();
is $effects, 3, 'and so does a direct call';

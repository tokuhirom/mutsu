use Test;
use nqp;

plan 10;

# Explicit `Mu.clone` of a Block is a distinct code object that keeps the
# original's `state` scope; the ordinary `Block.clone` starts its own (#15897).
my $f = -> { state $n = 0; ++$n };
my $g = Mu.^lookup('clone')($f);
nok nqp::eqaddr(nqp::decont($f), nqp::decont($g)), 'Mu.clone makes a distinct code object';
is $f(), 1, 'the original counts 1';
is $g(), 2, 'the Mu.clone copy continues the same state';
is $f(), 3, 'and the original sees the copy\'s increment';

my $h = $f.clone;
nok nqp::eqaddr(nqp::decont($f), nqp::decont($h)), 'Block.clone makes a distinct code object';
is $h(), 1, 'Block.clone starts independent state';
is $f(), 4, 'the original is unaffected by the Block.clone copy';

# The shared scope outlives whichever copy is dropped first.
my $a = -> { state $c = 10; ++$c };
my $b = Mu.^lookup('clone')($a);
$a();
$a = Nil;
is $b(), 12, 'the copy keeps the state after the original is dropped';
my $c = Mu.^lookup('clone')($b);
$b = Nil;
is $c(), 13, 'a copy of a copy shares the same scope';

# A Mu method object runs Mu's row, not the invocant class's override.
class K { method clone { 'K.clone' } }
is Mu.^lookup('clone')(K.new).^name, 'K', 'Mu.clone of an instance is the representation copy';

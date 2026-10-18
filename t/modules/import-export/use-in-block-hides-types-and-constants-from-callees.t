use Test;

# ADR-8751 / #13604: a top-level routine that names symbols by string
# (`::('Name')`, `EVAL`) resolves them in its own lexical outer. A class or
# constant that a block-scoped `use` imported is not there, so the routine
# does not see it when the block calls it; a same-named symbol of the
# routine's own outer still answers. Checked against rakudo.
plan 13;

use lib 't/lib';

class UibTop { method hi { 'top' } }
constant UIB-TOP = 7;

sub callee-type    { (try ::('UibFoo').hi) // 'no type' }
sub callee-const   { (try EVAL q[UIB-KONST]) // 'no constant' }
sub callee-lower   { (try EVAL q[uib-lower]) // 'no constant' }
sub callee-eval-type { (try EVAL q[UibFoo.hi]) // 'no type' }
sub callee-own     { ::('UibTop').hi ~ ' ' ~ EVAL(q[UIB-TOP]) }

{
    use UseInBlockTypesConsts;
    is UibFoo.hi, 'imported', 'the block sees the imported class';
    is UIB-KONST, 42, 'and the imported constant';
    is ::('UibFoo').hi, 'imported', 'also by name';
    is EVAL(q[UIB-KONST]), 42, 'and through an EVAL in the block';
    is callee-type(), 'no type', 'a callee does not see the imported class';
    is callee-eval-type(), 'no type', 'nor the class through an EVAL';
    is callee-const(), 'no constant', 'nor the imported constant';
    is callee-lower(), 'no constant', 'nor a lowercase one';
    is callee-own(), 'top 7', 'it still sees its own outer\'s symbols';
}
is (try ::('UibFoo').hi) // 'no type', 'no type', 'after the block the class is gone';
is (try EVAL q[UibFoo.hi]) // 'no type', 'no type', 'nor through an EVAL after the block';
my &keeps-type;
{
    use UseInBlockTypesConsts;
    &keeps-type = -> { UibFoo.hi };
}
is keeps-type(), 'imported', 'a closure taken in the block keeps the class';
is (try EVAL q[UibFoo.hi]) // 'no type', 'no type', 'a second block\'s import ends with it too';

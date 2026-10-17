use Test;

# ADR-8751 stage 2: the user candidates of the built-in set operators that a
# block-scoped `use` imports (`multi infix:<(|)>(Bx, Bx)`) add to the
# operator inside the block only. `(elem)` is a built-in too: a call its
# candidates do not take is the built-in's. Checked against rakudo.
plan 11;

use lib 't/lib';

sub outside-union($a, $b) { ([(|)] $a, $b) ~~ Set ?? 'built-in' !! 'candidate' }
sub outside-elem($a, $b) { ($a, $b).reduce(&infix:<(elem)>) }
my &keeps-union;
my ($x, $y);
{
    use UseInBlockSetOpLexical;
    $x = Bx.new(n => 1);
    $y = Bx.new(n => 2);
    is ($x (|) $y).n, 3, 'the operator';
    is-deeply (1, 2) (|) (3,), set(1, 2, 3), 'the built-in still answers what no candidate takes';
    is ([(|)] $x, $y).n, 3, 'a reduction';
    is (($x,) »(|)« ($y,)).map(*.n).join, '3', 'a hyper';
    is &infix:<(|)>($x, $y).n, 3, 'the operator as a value';
    is $x (elem) $y, 'custom-elem', '(elem)';
    ok 1 (elem) (1, 2), 'the built-in (elem)';
    is outside-union($x, $y), 'built-in', 'a reduction in a callee written outside the block';
    nok outside-elem($x, $y), '&infix:<(elem)> in a callee';
    &keeps-union = -> $a, $b { ($a (|) $b).n };
}
is keeps-union($x, $y), 3, 'a closure keeps the candidates after the block';
is (try ($x (|) $y).^name) // 'none', 'Set', 'after the block the operator is the built-in alone';

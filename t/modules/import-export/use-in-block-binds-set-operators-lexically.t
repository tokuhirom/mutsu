use Test;

# ADR-8751 stage 2: the user candidates of the built-in set operators that a
# block-scoped `use` imports (`multi infix:<(|)>(Bx, Bx)`) add to the
# operator inside the block only. `(elem)` is a built-in too: a call its
# candidates do not take is the built-in's. So do the Unicode spellings
# (`∪`) and the set relations (`⊆`, `(<=)`), whose uses in the block call the
# imported operator. Checked against rakudo.
plan 25;

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
    is-deeply ([(elem)] $x, $y), True, 'a reduction over a candidate chains to a Bool';
    is outside-union($x, $y), 'built-in', 'a reduction in a callee written outside the block';
    nok outside-elem($x, $y), '&infix:<(elem)> in a callee';
    &keeps-union = -> $a, $b { ($a (|) $b).n };
}
is keeps-union($x, $y), 3, 'a closure keeps the candidates after the block';
is (try ($x (|) $y).^name) // 'none', 'Set', 'after the block the operator is the built-in alone';

# The Unicode spellings and the set relations.
sub outside-cup($a, $b) { ($a ∪ $b).^name }
sub outside-cup-reduce($a, $b) { ([∪] $a, $b).^name }
my &keeps-cup;
my ($p, $q);
{
    use UseInBlockSetSpellingLexical;
    $p = By.new(n => 1);
    $q = By.new(n => 2);
    is ($p ∪ $q).n, 3, 'a Unicode spelling';
    is ([∪] $p, $q).n, 3, 'a reduction over the Unicode spelling';
    is (($p,) »∪« ($q,)).map(*.n).join, '3', 'a hyper over the Unicode spelling';
    is-deeply set(1) ∪ set(2), set(1, 2), 'the built-in Unicode spelling still answers';
    is &infix:<∪>(set(1), set(3)).^name, 'Set', 'the built-in through the operator value';
    is $p ⊆ $q, 'custom-subset', 'a set relation';
    is $p (<=) $q, 'custom-ascii-subset', 'an ASCII set relation';
    ok set(1) ⊆ set(1, 2), 'the built-in set relation still answers';
    ok ([⊆] set(1), set(1, 2), set(1, 2, 3)), 'a reduction over the relation chains';
    is-deeply ([\⊆] set(1), set(1, 2), set(1)), (True, True, False).Seq,
        'a triangle reduction over the relation chains';
    is outside-cup($p, $q), 'Set', 'a callee written outside the block';
    is outside-cup-reduce($p, $q), 'Set', 'a reduction in a callee written outside the block';
    &keeps-cup = -> $a, $b { ($a ∪ $b).n };
}
is keeps-cup($p, $q), 3, 'a closure keeps the Unicode spelling after the block';

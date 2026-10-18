use Test;

# ADR-8751 stage 2: the user candidates of the built-in repetition operators
# (`x`, `xx`), the built-in postfix `i` and the built-in subscripts
# (`postcircumfix:<[ ]>`, `postcircumfix:<{ }>`) that a block-scoped `use`
# imports add to the operator inside the block only. A callee written outside
# the block does not see them, a closure taken in the block keeps them, and
# what no candidate accepts is the built-in's. Checked against rakudo, except
# where noted.
plan 24;

use lib 't/lib';

sub outside-x($a, $b) { $a x $b }
sub outside-i($a) { (try $a\i) // 'built-in' }
sub outside-pos($a) { $a[0] }
sub outside-key($a) { $a<k> }
my (&keeps-x, &keeps-i, &keeps-pos, &keeps-key);
my $r;
{
    use UseInBlockRepeatPostfixLexical;
    $r = Rx.new(n => 1);
    is $r x $r, 'custom-x', 'infix x';
    is 'ab' x 2, 'abab', 'the built-in x still answers what no candidate takes';
    is ([x] $r, $r), 'custom-x', 'a reduction over x';
    is-deeply (($r,) »x« ($r,)), ('custom-x',), 'a hyper over x';
    is &infix:<x>($r, $r), 'custom-x', 'x as a value';
    my $s = $r;
    $s x= $r;
    is $s, 'custom-x', 'x=';
    is-deeply ('a' xx 2), ('a', 'a').Seq, 'the built-in xx still answers';
    is $r\i, 'custom-i', 'postfix i';
    is 3\i, 3i, 'the built-in postfix i still answers';
    is $r[3], 'custom-at-pos-3', 'postcircumfix [ ]';
    is $r{'k'}, 'custom-at-key-k', 'postcircumfix { }';
    is $r<z>, 'custom-at-key-z', 'an angle subscript';
    is @($r, $r)[1][2], 'custom-at-pos-2', 'a chained subscript';
    is (1, 2, 3)[1], 2, 'the built-in [ ] still answers';
    is {a => 1}<a>, 1, 'the built-in { } still answers';
    is &postcircumfix:<[ ]>((5, 6), 1), 6, '&postcircumfix:<[ ]> falls back to the built-in';
    is outside-x($r, $r), 'rr', 'x in a callee written outside the block';
    isnt outside-i($r), 'custom-i', 'postfix i in a callee written outside the block';
    isnt outside-pos($r), 'custom-at-pos-0', '[ ] in a callee written outside the block';
    is outside-key($r), 'built-in-at-key-k', '{ } in a callee written outside the block';
    &keeps-x = -> $a, $b { $a x $b };
    &keeps-i = -> $a { $a\i };
    &keeps-pos = -> $a { $a[5] };
    &keeps-key = -> $a { $a<q> };
}
is keeps-x($r, $r), 'custom-x', 'a closure keeps x after the block';
is keeps-i($r), 'custom-i', 'a closure keeps postfix i after the block';
is keeps-pos($r), 'custom-at-pos-5', 'a closure keeps [ ] after the block';
is keeps-key($r), 'custom-at-key-q', 'a closure keeps { } after the block';

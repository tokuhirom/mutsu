use v6;
use Test;

# `Rakudo::Iterator.ReifiedReverse($buffer, $what)` and `.ReifiedRotate($n,
# $buffer, $what)` iterate a reified IterationBuffer reversed / rotated.
# ParaSeq's `reverse` and `rotate` build their Seq on them. Identical under raku.

plan 5;

my \ib = IterationBuffer.new;
ib.push($_) for 1..5;

is Seq.new(Rakudo::Iterator.ReifiedReverse(ib, Mu)).list.raku, '(5, 4, 3, 2, 1)', 'reverse';
is Seq.new(Rakudo::Iterator.ReifiedRotate(2, ib, Mu)).list.raku, '(3, 4, 5, 1, 2)', 'rotate by 2';
is Seq.new(Rakudo::Iterator.ReifiedRotate(-1, ib, Mu)).list.raku, '(5, 1, 2, 3, 4)', 'rotate by -1';
is Seq.new(Rakudo::Iterator.ReifiedRotate(0, ib, Mu)).list.raku, '(1, 2, 3, 4, 5)', 'rotate by 0';
is Seq.new(Rakudo::Iterator.ReifiedReverse(IterationBuffer.new, Mu)).list.elems, 0, 'an empty buffer';

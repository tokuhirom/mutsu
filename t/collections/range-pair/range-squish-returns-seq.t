use Test;

# `Range.squish` answers a Seq of the squished elements, not the Range itself (#16038).
plan 7;

is (0..3).squish.raku, '(0, 1, 2, 3).Seq', 'closed range';
is (0..^3).squish.raku, '(0, 1, 2).Seq', 'excluded end';
is (1.5..4).squish.raku, '(1.5, 2.5, 3.5).Seq', 'Rat start';
is ("a".."c").squish.raku, '("a", "b", "c").Seq', 'string range';
is (1..Inf).squish[^3].raku, '(1, 2, 3)', 'infinite range stays lazy';
ok (1..*).squish.is-lazy, 'infinite squish is lazy';
class P is Range {}
is P.new(0, 3).squish.raku, '(0, 1, 2, 3).Seq', 'Range subclass';

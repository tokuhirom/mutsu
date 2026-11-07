# `Seq.new($iterator).skip(*-3)` (a Callable or Whatever count) keeps the Seq unspent for the
# eager implementation when the lazy stage declines it (#17037).
use Test;
plan 6;

is Seq.new((1..10).iterator).skip(*-3).list.raku, '(8, 9, 10)', 'skip with a WhateverCode count';
is Seq.new((1..10).iterator).skip({ $_ - 3 }).list.raku, '(8, 9, 10)', 'skip with a block count';
is Seq.new((1..10).iterator).skip(*).list.raku, '()', 'skip with a bare Whatever';
is Seq.new((1..10).iterator).skip(3).list.raku, '(4, 5, 6, 7, 8, 9, 10)', 'skip with an Int count (the lazy stage)';
is Seq.new((1..10).iterator).skip(2).skip(*-3).list.raku, '(8, 9, 10)', 'a stage followed by a Callable skip';
my $s = Seq.new((1..4).iterator);
is $s.skip(*-1).list.raku, '(4,)', 'the Seq is consumed by the one skip';

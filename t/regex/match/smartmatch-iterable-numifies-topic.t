use Test;

# `Numeric.ACCEPTS` compares `topic == self`, so an aggregate topic
# numifies through its own `.Numeric`: a list, hash, Set or Seq to its element
# count, a Bag or Mix to its total weight (#17915). `Range.ACCEPTS` compares a
# list's, Seq's or hash's element count the same way.

plan 16;

ok  set(1, 2) ~~ 2, 'a Set ~~ its element count';
nok set(1, 2) ~~ 3, '... not another number';
ok  set() ~~ 0, 'an empty Set ~~ 0';
ok  bag(1, 1, 2) ~~ 3, 'a Bag ~~ its total weight';
ok  (a => 1.5, b => 1).Mix ~~ 2.5, 'a Mix ~~ its total weight';
ok  (1, 2).Seq ~~ 2, 'a Seq ~~ its element count';
my $s = (1, 2).Seq;
ok  $s ~~ 2, '... a Seq held in a variable';
is  $s.list, (1, 2), '... which stays readable';
ok  (gather { take 1; take 2 }) ~~ 2, 'a gather';
ok  { a => 1, b => 2 } ~~ 2, 'a Hash ~~ its element count';
ok  [1, 2] ~~ 2, 'an Array, as before';
my $two = 2;
ok  set(1, 2) ~~ $two, 'a matcher held in a variable';

ok  (1, 2).Seq ~~ 1..3, 'a Seq ~~ a Range by its element count';
ok  (1, 2).Seq ~~ 2..2, '... at the bound';
ok  { a => 1, b => 2 } ~~ 2..2, 'a Hash ~~ a Range';
nok [1, 2, 3] ~~ 1..2, 'an Array beyond the Range';

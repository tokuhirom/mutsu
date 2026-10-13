use Test;

# ADR-11276 §9.99: `.Rat($epsilon)` and `.FatRat($epsilon)` are rows of the
# types Rakudo declares them on (`Int`, `Num`, `Rat`, `Complex`, and an
# allomorph through its numeric value). `Cool.Rat` and `Cool.FatRat` take no
# epsilon, so a `Str`, a `List` or a `Match` refuses one; the 1-arg cascade
# arms that answered them (a `Str` leniently, a `List` as `0`) are gone.
# The exception type is Rakudo's bind failure there, not yet mutsu's (#13203).

plan 14;

is 3.14159e0.Rat(0.01).raku, '<22/7>', 'Num.Rat($epsilon)';
is 3.14159e0.FatRat(0.01).raku, 'FatRat.new(22, 7)', 'Num.FatRat($epsilon)';
is 5.Rat(0.1).raku, '5.0', 'Int.Rat($epsilon)';
is 2.5.FatRat(0.1).raku, 'FatRat.new(5, 2)', 'Rat.FatRat($epsilon)';
is <3.14159e0>.Rat(0.01).raku, '<22/7>', 'NumStr.Rat($epsilon) is its Num\'s';
is <3>.FatRat(0.01).raku, 'FatRat.new(3, 1)', 'IntStr.FatRat($epsilon) is its Int\'s';
is (3.14159+0i).Rat(0.01).raku, '<22/7>', 'Complex.Rat($epsilon)';

is "3.5".Rat.raku, '3.5', 'Str.Rat with no epsilon is Cool.Rat';
dies-ok { "3.14159".Rat(0.01) }, 'Cool.Rat takes no epsilon: a Str refuses one';
dies-ok { "1/3".FatRat(0.1) }, '... nor does Cool.FatRat';
dies-ok { (1, 2, 3).Rat(0.1) }, '... a List refuses one';
dies-ok { Str.Rat(0.1) }, '... and so does a Str type object';
my $m = "2.5" ~~ /.+/;
is $m.Rat.raku, '2.5', 'Match.Rat is Cool.Rat on the matched text';
dies-ok { $m.Rat(0.1) }, '... and refuses an epsilon';

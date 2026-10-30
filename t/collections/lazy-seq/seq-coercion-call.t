use Test;

# Data::Dump::Tree's t/43_sequences.rakutest calls `Seq(1, 2, 'x')`: the
# coercion-type call of Seq, which makes a Seq of its arguments (one argument
# is its `.Seq`, no argument is the coercion type term `Seq(Any)`).

plan 9;

is Seq(1, 2, 'x').raku, '(1, 2, "x").Seq', 'several arguments are the elements of a Seq';
isa-ok Seq(1, 2, 'x'), Seq, 'the result is a Seq';
is Seq((1, 2, 3)).raku, '(1, 2, 3).Seq', 'one List argument is its .Seq';
is Seq(1..3).raku, '(1, 2, 3).Seq', 'one Range argument is its .Seq';
is Seq(1).raku, '(1,).Seq', 'one scalar argument is a one-element Seq';
my @a = 5, 6;
is Seq(@a).raku, '(5, 6).Seq', 'one Array argument contributes its elements';
is Seq().raku, 'Seq(Any)', 'no argument is the coercion type term';
is Seq(Int).raku, 'Seq(Int)', 'a lone type object is a parametric coercion type';
is Seq(Seq(1, 2)).raku, '(1, 2).Seq', 'a Seq argument stays a Seq';

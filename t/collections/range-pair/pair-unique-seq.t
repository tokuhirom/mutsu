use Test;

plan 6;

my $named = a => 5;
is $named.unique.raku, '(:a(5),).Seq', 'a string-keyed Pair unique is a singleton Seq';
is $named.unique.^name, 'Seq', 'a string-keyed Pair unique returns Seq';
is $named.unique[0] === $named, True, 'the singleton Seq keeps the original Pair';

my $data = 1 => 'x';
is $data.unique.raku, '(1 => "x",).Seq', 'a data-key Pair unique is a singleton Seq';
is $data.unique.^name, 'Seq', 'a data-key Pair unique returns Seq';
is $data.unique[0] === $data, True, 'the singleton Seq keeps the original data Pair';

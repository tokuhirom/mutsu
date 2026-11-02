use Test;

# From the Format::Lisp ecosystem distribution (its ~a directive pads a value
# with `.chars`): a Range and a Seq are Cool, so `.chars` is the length of
# their string form. It used to die with "No such method 'chars'".

plan 8;

is (1..3).chars, 5, '"1..3".chars';
is (1..^3).chars, 3 + 0, '"1..^3".chars is the length of its Str form';
is (1^..3).chars, 3, '"1^..3".chars';
is ('a'..'c').chars, 5, 'a string Range';
is (1.5..3.5).chars, 11, 'a Rat/Num Range';
is (^3).chars, 5, 'the ^N prefix form';
is (1, 2, 3).map(* + 1).Seq.chars, 5, 'a Seq: its Str form is "2 3 4"';
is (1..3).reverse.chars, 5, 'a reversed Range is a Seq';

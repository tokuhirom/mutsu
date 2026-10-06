use Test;

# An allomorph invocant answers `.Rat(eps)` / `.FatRat(eps)` with its numeric
# value (#12097); it used to answer 0. Answers are Rakudo's.

plan 7;

is <7>.Rat("0.01").raku, '7.0', 'IntStr.Rat(Str)';
is <7>.Rat.raku, '7.0', 'IntStr.Rat';
is <7>.FatRat("0.01").raku, 'FatRat.new(7, 1)', 'IntStr.FatRat(Str)';
is <3.5>.FatRat(0.01).raku, 'FatRat.new(7, 2)', 'RatStr.FatRat(Real)';
is <3.14159e0>.Rat(0.01).raku, '<22/7>', 'NumStr.Rat(Real)';
is <3.14159e0>.FatRat(0.01).raku, 'FatRat.new(22, 7)', 'NumStr.FatRat(Real)';
dies-ok { <3.5e0>.Rat("0.01") }, 'NumStr.Rat(Str) binds the epsilon';

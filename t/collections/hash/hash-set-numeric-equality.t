use Test;

# Found via the Math::Matrix distribution (t/022-converter.rakutest):
# `Map.Numeric` / `Setty.Numeric` are `.elems`, so `==` on two of them
# compares entry counts.

plan 6;

my %a = 1 => 2, 3 => 4;
my %b = 0 => { 0 => 1, 1 => 2 }, 1 => { 0 => 3, 1 => 4 };

ok %a == %b, 'two Hashes with two entries are ==';
ok %a == { x => 1, y => 2 }, 'Hash == anonymous Hash by entry count';
nok %a == { x => 1 }, 'Hashes with different entry counts are not ==';
ok set(1, 2) == set(3, 4), 'two Sets with two elements are ==';
nok set(1, 2) == set(3), 'Sets with different sizes are not ==';
ok %a.Numeric == %b.Numeric, 'agrees with the explicit .Numeric';

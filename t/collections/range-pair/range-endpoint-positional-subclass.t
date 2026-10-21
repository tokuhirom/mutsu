use Test;

# A Range endpoint that is an Array/Hash subclass instance or a role mixin of
# one numifies to its element count, as a plain Array does: `^$a` is `^3`.
# Found by PDF's `^max(@!index, self)` over a `PDF::COS::Array`. Expected
# values are the `raku` (v2026.09) output.

plan 8;

class MyA is Array {}
my $a = MyA.new(1, 2, 3);
is (^$a).raku, '^3', 'prefix ^ of an Array subclass instance';
is (0..^$a).raku, '^3', '0..^ of it';
is (0..$a).raku, '0..3', '0.. of it';
is (1..$a).raku, '1..3', '1.. of it';
is (^$a).elems, 3, 'the range has the element count';

my $b = [1, 2] but role R {};
is (^$b).raku, '^2', 'prefix ^ of a role mixin of an Array';

class MyH is Hash {}
my $h = MyH.new;
$h<a> = 1;
is (^$h).raku, '^1', 'prefix ^ of a Hash subclass instance';

my $n = 0;
for ^$a { $n += $_ }
is $n, 3, 'iterating it';

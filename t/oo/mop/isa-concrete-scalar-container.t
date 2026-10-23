use Test;

# `.^isa` of a concrete container (`$x.VAR`, a `Scalar` with no class entry of
# its own) follows the ancestry `.^mro` reports (#13264, AttrX::Mooish `t/010-base`).

plan 6;

my $x = 3;
my \v = $x.VAR;
is v.^isa(Scalar), 1, 'a Scalar container isa Scalar';
is v.^isa(Any), 1, 'a Scalar container isa Any';
is v.^isa(Mu), 1, 'a Scalar container isa Mu';
is v.^isa(Int), 0, 'a Scalar container is not an Int';
is v.^isa(Cool), 0, 'a Scalar container is not a Cool';

my @a = 1, 2;
is @a.VAR.^isa(Array), 1, 'an Array container isa Array';

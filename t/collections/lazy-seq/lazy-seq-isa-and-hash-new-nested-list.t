use Test;
# From the Listicles distribution (t/01-basic.rakutest).
plan 8;

my $g = gather { take 1; take 2 };
ok $g.isa(Seq), 'gather Seq .isa(Seq)';
ok $g.isa("Seq"), 'gather Seq .isa("Seq")';
ok (1, 2).map({ $_ }).isa(Seq), 'map Seq .isa(Seq)';
nok $g.isa(Array), 'gather Seq is not an Array';

my @a = 1, 2, "4";
is-deeply Hash.new((0..2).map({ $_, @a[$_] })), {0 => 1, 1 => 2, 2 => "4"},
    'Hash.new flattens a Seq of Lists like *@';
is-deeply Hash.new(((0, 1), (1, 2))), {0 => 1, 1 => 2}, 'nested plain Lists flatten';
is-deeply Hash.new([1, 2], [3, 4]), {1 => 2, 3 => 4}, 'Array args flatten one level';
is-deeply Hash.new((1, $(2, 3))).elems, 1, 'itemized List stays whole';

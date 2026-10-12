use Test;

plan 9;

# A `$`-variable holds its value as one item, so a Seq or Range read from one
# is a single element of a `*@` parameter; the same value passed directly
# (not through a variable) still flattens (#12970).
sub count(*@a) { @a.elems }
sub shape(*@a) { @a.raku }

my $s = (1, 2).map({ $_ });
is count($s), 1, 'a Seq held in a scalar is one element';
is shape($s), '[(1, 2).Seq,]', 'and it is still the Seq';

is count((1, 2).map({ $_ })), 2, 'the same Seq passed directly flattens';

my $t = (1, 2).map({ $_ });
is count($t, 5), 2, 'a scalar-held Seq plus another argument';

my $r = 1..3;
is count($r), 1, 'a Range held in a scalar is one element';
is count(1..3), 3, 'a Range passed directly flattens';

my $g = gather { take 1; take 2 };
is count($g), 1, 'a gather held in a scalar is one element';

my @arr = 1, 2;
is count(@arr), 2, 'an array still flattens';
my $item = @arr;
is count($item), 1, 'an array held in a scalar is one element';

done-testing;

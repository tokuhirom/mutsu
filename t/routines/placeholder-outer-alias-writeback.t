use Test;

plan 2;

my $a = 1..3;
my ( &compare, &unused ) = do { ( &infix:«<=>», &infix:«>=» ) };
my @sorted = (8..11, 2..7).sort({ &compare($^a.min, $^b.min) });

is $a.raku, '1..3', 'a placeholder alias does not write back to an outer lexical';
is @sorted[0].raku, '2..7', 'a closure using a placeholder alias compares correctly';

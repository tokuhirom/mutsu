use Test;

plan 2;

my @a = [1, 2], [3, 4];
@a[0;1] := 9;
is @a[0;1], 9, 'semicolon dimensions bind the selected nested element';

my $x = 5;
@a[1;0] := $x;
$x = 8;
# rakudo 2026.11 binds the source's container (#18541).
is @a[1;0], 8, 'semicolon indexing binds the source container';

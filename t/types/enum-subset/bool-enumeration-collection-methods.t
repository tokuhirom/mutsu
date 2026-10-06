use Test;

plan 12;

my $enum = True;

is $enum.keys.sort.raku, '("False", "True").Seq', 'Bool keys are the enum names';
is $enum.values.sort.raku, '(0, 1).Seq', 'Bool values are the enum numeric values';
is $enum.kv.sort.raku, '(0, 1, "False", "True").Seq', 'Bool kv contains both enum entries';
is $enum.pairs.map(*.raku).sort.raku, '(":False(0)", ":True(1)").Seq', 'Bool pairs are named enum pairs';
is $enum.antipairs.map(*.raku).sort.raku, '("0 => \\"False\\"", "1 => \\"True\\"").Seq', 'Bool antipairs reverse each enum entry';
is $enum.minpairs.raku, '(:False(0),)', 'Bool minpairs returns the False enum pair in a List';
is $enum.minpairs.^name, 'List', 'Bool minpairs returns a List';
is $enum.maxpairs.raku, '(:True(1),)', 'Bool maxpairs returns the True enum pair in a List';
is $enum.unique.raku, '(Bool::True,).Seq', 'Bool unique is a singleton Seq';
is $enum.unique.^name, 'Seq', 'Bool unique returns Seq';

my $false = False;
is $false.keys.sort.raku, '("False", "True").Seq', 'False has the same enum keys';
is $false.unique.raku, '(Bool::False,).Seq', 'False unique retains its value';

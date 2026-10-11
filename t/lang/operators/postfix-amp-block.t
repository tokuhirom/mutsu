use Test;

plan 4;

is 5.&{ $_ * 2 }, 10, '.&{ } calls the block with the invocant as $_';
is (1, 2).map({ .&{ $_ * 2 } }).raku, (2, 4).Seq.raku, '.&{ } inside map';
is 3.&{ $^a + 1 }, 4, '.&{ } with a placeholder parameter';
is 7.&{ $^a + $^b }(1), 8, '.&{ }(args) passes extra arguments';

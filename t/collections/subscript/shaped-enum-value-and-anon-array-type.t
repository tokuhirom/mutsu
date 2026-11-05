use Test;

enum Compass <North East South West>;
my @single[West];
is-deeply @single.shape, (3,), 'an enum value is its numeric value as a shaped dimension';

my @grid[West; West];
is-deeply @grid.shape, (3, 3), 'enum values are the size of each shaped dimension';

is (my int @).of, int, 'anonymous native array retains its element type';
is (my uint8 @ = 1, 2).of, uint8, 'initialized anonymous native array retains its element type';
is (my Str @).of, Str, 'anonymous boxed array retains its element type';

done-testing;

use Test;

enum Compass <North East South West>;
my @single[West];
is-deeply @single.shape, (4,), 'enum value sizes one shaped dimension';

my @grid[West; West];
is-deeply @grid.shape, (4, 4), 'enum values size each shaped dimension';

is (my int @).of, int, 'anonymous native array retains its element type';
is (my uint8 @ = 1, 2).of, uint8, 'initialized anonymous native array retains its element type';
is (my Str @).of, Str, 'anonymous boxed array retains its element type';

done-testing;

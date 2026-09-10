use Test;

plan 10;

# Track B T4: multidim cas runs through the celled atomic store (the same
# element-cell representation 1-dim cas uses), so mixing the two on one array
# stays coherent, and reads/assigns through the celled store keep working.

# Plain nested array: multidim cas swaps and the swap is visible.
my @plain = [1,2],[3,4];
is cas(@plain[1;1], 4, 9), 4, 'multidim cas returns the observed value';
is @plain[1;1], 9, 'multidim cas swap is visible through a multidim read';

# Shaped array: same through the shaped declaration.
my @matrix[2;2];
@matrix[1;1] = 5;
is cas(@matrix[1;1], 5, 7), 5, 'shaped multidim cas returns the observed value';
is @matrix[1;1], 7, 'shaped multidim cas swap is visible';

# Mixing 1-dim and multidim cas on the same array: a 1-dim cas cells the
# store; the multidim cas must read/write through those cells (the old
# whole-array republish path read a plain element and returned 0 here).
my @mixed = 10, [20, 30];
is cas(@mixed[0], 10, 11), 10, '1-dim cas on the mixed array works';
is cas(@mixed[1;0], 20, 21), 20, 'multidim cas after a 1-dim cas observes the element';
is @mixed[1;0], 21, 'multidim swap landed through the cell';
is @mixed[0], 11, '1-dim swap still visible';

# Assign through the celled store after cas has celled it.
@mixed[1;1] = 33;
is @mixed[1;1], 33, 'multidim assign writes through the celled store';

# Threaded multidim cas: concurrent increments all land.
my atomicint @values[2;2];
@values[1;1] = 0;
await start {
    for 1..500 -> int $i {
        loop {
            my int $orig = @values[1;1];
            last if cas(@values[1;1], $orig, $orig + $i) == $orig;
        }
    }
} xx 4;
is @values[1;1], 4 * [+](1..500), 'concurrent multidim cas increments all land';

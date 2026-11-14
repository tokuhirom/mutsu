use Test;

# A multi-dimensional element read (`@a[1;1]`) beside another thread's
# `cas(@a[1;1], ...)` on the same array. The first `cas` promotes the row
# `@a[1]` to a shared element cell and later ones re-seat the cell's row; the
# multi-dim reader used to walk the live row through a borrowed slot without
# the container's structure lock, and could read the slot mid-promotion or a
# row already freed: `Cannot unbox a type object (Nil) to int`, a garbage
# Num, or a wrong element (#17781, roast/S17-lowlevel/cas-int.t).

plan 2;

my $undefined = 0;
my $wrong-sum = 0;
for 1..200 -> $attempt {
    my atomicint @values[2;2];
    @values[1;1] = 0;
    await start {
        for 1..30 -> int $i {
            loop {
                my $seen = @values[1;1];
                unless $seen ~~ Int:D {
                    $undefined++;
                    last;
                }
                my int $orig = $seen;
                last if cas(@values[1;1], $orig, $orig + $i) == $orig;
            }
        }
    } xx 4;
    $wrong-sum++ unless @values[1;1] == 4 * [+](1..30);
}
is $undefined, 0, 'a multi-dim read beside a concurrent cas always sees the Int element';
is $wrong-sum, 0, 'every cas on a 2-dim atomicint element lands';

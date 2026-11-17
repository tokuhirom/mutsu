use v6;
use Test;

# A Bool stored into a native `int`/`int64` declaration unboxes to 0/1 (Bool is
# an Int enum); a `num` still rejects it. The loop shape is the one a native
# flag computed per iteration takes.

plan 7;

my int $t = True;
is $t, 1, 'True into a native int';
is $t.WHAT.^name, 'Int', 'and the value is an Int';
my int64 $f = 3 < 2;
is $f, 0, 'a False comparison into int64';

my @flags;
loop (my int $i = 0; $i < 4; $i++) {
    my int $even = $i %% 2;
    @flags.push: $even;
}
is-deeply @flags, [1, 0, 1, 0], 'a native flag declared per loop iteration';

my int $sum = 0;
for ^10 { my int $hit = $_ > 6; $sum += $hit }
is $sum, 3, 'summing per-iteration native flags';

dies-ok { my num $n = True }, 'a num rejects a Bool';
my Int $boxed = True;
is $boxed.WHAT.^name, 'Bool', 'a boxed Int keeps the Bool';

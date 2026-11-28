use Test;

# A loop kernel reads `@a[$i]` and `@grid[$i][$j]` in place, through a `:=`
# binding cell when the array has one. Every read must give what the generic
# loop gives, and every element the kernel cannot hold (a hole, a Str, a
# container, a big Int) must still be read right after the kernel declines.

plan 9;

sub celled-sum() {
    my @data = ^256;
    my @alias := @data;
    my $s = 0;
    for ^5000 -> $i { $s += @alias[$i +& 255] }
    $s
}
is celled-sum(), 629340, 'Int reads through a := binding cell';

sub plain-sum() {
    my @data = ^64;
    my $s = 0;
    for ^1000 -> $i { $s += @data[$i +& 63] }
    $s
}
is plain-sum(), 31020, 'Int reads of a plain array';

sub num-sum() {
    my @data = (^8).map(* + 0.5e0);
    my @alias := @data;
    my $s = 0e0;
    for ^80 -> $i { $s += @alias[$i +& 7] }
    $s
}
is num-sum(), 320e0, 'Num reads through a binding cell';

sub grid-sum() {
    my @grid;
    @grid[$_] = [^8] for ^8;
    my $s = 0;
    for ^640 -> $k { $s += @grid[$k +& 7][($k +> 3) +& 7] }
    $s
}
is grid-sum(), 2240, 'two-level reads';

sub with-hole() {
    my @data = 1, 2, 3;
    @data[5] = 6;
    my $n = 0;
    for ^6 -> $i { $n++ if @data[$i].defined }
    $n
}
is with-hole(), 4, 'a hole is read as undefined';

sub mixed() {
    my @data = 1, 2, '3', 4;
    my @alias := @data;
    my $s = 0;
    for ^4 -> $i { $s += @alias[$i] }
    $s
}
is mixed(), 10, 'a Str element among Ints';

sub big() {
    my @data = 1, 2**70, 3;
    my $s = 0;
    for ^3 -> $i { $s += @data[$i] }
    $s
}
is big(), 2**70 + 4, 'a big Int element';

sub rebound() {
    my @a = 1 xx 4;
    my @b = 2 xx 4;
    my @x := @a;
    my $s = 0;
    for ^4 -> $i { $s += @x[$i] }
    @x := @b;
    for ^4 -> $i { $s += @x[$i] }
    $s
}
is rebound(), 12, 'a rebound alias reads its new array';

sub written() {
    my @data = 0 xx 4;
    my @alias := @data;
    @data[2] = 40;
    my $s = 0;
    for ^4 -> $i { $s += @alias[$i] }
    $s
}
is written(), 40, 'a store through the other name is seen';

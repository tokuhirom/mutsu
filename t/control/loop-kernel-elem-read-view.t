use Test;

# A loop kernel reads the arrays its region only reads through a view of
# their elements resolved at entry. Every shape that could make that view
# stale -- a store into the same array under another name, a store growing
# another array, an out-of-range read, a later entry after the array changed
# -- must still read what the generic loop reads.

plan 8;

sub copy-scaled() {
    my @src = ^64;
    my @dst = 0 xx 64;
    for ^64 -> $i { @dst[$i] = @src[$i] * 2 }
    [+] @dst
}
is copy-scaled(), 4032, 'read one array, store another';

sub grow-other() {
    my @src = ^8;
    my @dst;
    for ^8 -> $i { @dst[$i] = @src[$i] + 1 }
    @dst.elems ~ ':' ~ [+] @dst
}
is grow-other(), '8:36', 'the stored array grows while the read one is viewed';

sub aliased() {
    my @a = 1 xx 8;
    my @b := @a;
    for ^8 -> $i { @b[$i] = @a[$i] + $i }
    [+] @a
}
is aliased(), 36, 'a store through another name of the read array';

sub out-of-range() {
    my @a = ^4;
    my $n = 0;
    for ^5000 -> $i { $n++ if @a[$i].defined }
    $n
}
is out-of-range(), 4, 'reads past the end';

sub re-entered() {
    my @a = 0 xx 4;
    my $s = 0;
    for ^3 -> $round {
        for ^3000 -> $i { $s += @a[$i +& 3] }
        @a = (1 + $round) xx 6;
    }
    $s
}
is re-entered(), 9000, 'each entry sees the array as it is then';

sub two-reads() {
    my @a = ^16;
    my @b = (^16).map(* * 10);
    my $s = 0;
    for ^5000 -> $i { $s += @a[$i +& 15] + @b[15 - ($i +& 15)] }
    $s
}
is two-reads(), 412788, 'two read arrays';

sub nums() {
    my @a = (^8).map(* / 2e0);
    my $s = 0e0;
    for ^8 -> $i { $s += @a[$i] }
    $s
}
is nums(), 14e0, 'Num elements';

sub celled-and-plain() {
    my @data = ^32;
    my @alias := @data;
    my @plain = ^32;
    my $s = 0;
    for ^5000 -> $i { $s += @alias[$i +& 31] - @plain[$i +& 31] }
    $s
}
is celled-and-plain(), 0, 'a celled and a plain array in one region';

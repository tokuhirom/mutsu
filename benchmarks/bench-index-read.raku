# Indexed element READ (issue #8308) — the companion to bench-index-store.
#
# bench-index-store closed half of the blind spot #8069 found: nothing in
# benchmarks/ WROTE through a subscript. Nothing read through one either.
# `array-ops` and `hash-access` measure grep/map/elems and whole-hash lookup
# routines, not `@a[$i]` and `%h{$k}` themselves, so the read path's own cost
# was as invisible as the store's had been — and on a read-dominated program it
# is the whole gap. `bench-threads-serial` does 600000 celled reads against
# 80000 stores; at the ~330 ns of marginal cost #8308 measured, the reads alone
# were ~200 ms of a ~790 ms run.
#
# Same discipline as bench-index-store: the SIMPLEST shape the read path has —
# a plain non-negative Int / Str key on a plain container, no type constraint,
# no default, no shape, no `:=`. Any cost here is cost the fast path is failing
# to avoid. The one deliberate exception is the celled section, which pins the
# `:=`-bound read that bench-threads-serial actually performs.
#
# The checksum is computed under mutsu and rakudo alike, so a wrong answer
# fails this file as loudly as a slow one.

my $checksum = 0;

# Array: read 16 slots of a 64-element array, 500000 times.
my @arr = ^64;
my $asum = 0;
loop (my $i = 0; $i < 500000; $i++) {
    $asum = $asum + @arr[$i +& 15];
}
$checksum += $asum;

# Hash: the associative twin, over 16 pre-existing keys, so what is measured is
# the lookup and not repeated growth or autovivification.
my %h;
%h{$_} = $_ for ^16;
my $hsum = 0;
loop (my $j = 0; $j < 200000; $j++) {
    $hsum = $hsum + %h{$j +& 15};
}
$checksum += $hsum;

# Nested: `@a[$i][$j]` is two reads through the same funnel, the read twin of
# bench-index-store's chained section.
my @grid;
@grid[$_] = [($_ * 8) ..^ ($_ * 8 + 8)] for ^8;
my $gsum = 0;
loop (my $k = 0; $k < 100000; $k++) {
    $gsum = $gsum + @grid[$k +& 7][($k +> 3) +& 7];
}
$checksum += $gsum;

# Celled: the `:=`-bound read bench-threads-serial performs, where the
# container arrives through a ContainerRef cell rather than a plain slot.
my @backing = ^64;
my @alias := @backing;
my $csum = 0;
loop (my $m = 0; $m < 200000; $m++) {
    $csum = $csum + @alias[$m +& 15];
}
$checksum += $csum;

say "checksum = $checksum";

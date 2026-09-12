# The single-threaded twin of bench-threads.raku (issue #8086).
#
# Same TOTAL work as bench-threads.raku -- 600000 celled reads and 80000 celled
# element stores, through the same `:=`-bound containers -- run on the mainline
# with no `start` anywhere. Read the two rows together:
#
#   bench-threads-serial                   how expensive a container op is
#   bench-threads / bench-threads-serial   how well it scales across cores
#
# Neither quantity is recoverable from bench-threads alone, which is how a
# defect with nothing to do with threads (the element store of #8069) came to
# dominate the only row in the suite that loses to rakudo.
#
# The `:=` bindings are load-bearing and must not be simplified away: a plain
# `my @a` is served by the name-keyed shared-variable lanes and would exercise
# neither the `ContainerRef` cell nor ADR-0068's read-side guard, so the twin
# would no longer be measuring the same storage path as bench-threads. See that
# file's header and #7613.

my $checksum = 0;

# Read-hot: the same 4 x 150000 reads bench-threads spreads over four workers,
# here on one thread. Every read of @alias and $sref goes through the cell.
# Kept as four 150000 loops rather than one 600000 loop so the checksum is
# IDENTICAL to bench-threads' -- 150000 is not a multiple of 256, so a single
# flat loop would sum a different set of elements and the two files could drift
# apart without anyone noticing.
my @data = ^256;
my @alias := @data;
my $seed = 7;
my $sref := $seed;

for ^4 {
    my $s = 0;
    for ^150000 -> $i {
        $s += @alias[$i +& 255] + $sref;
    }
    $checksum += $s;
}

# Write side: the same 4 x 20000 element stores, over the same 64 slots and the
# same disjoint 16-index blocks, so the surviving values match bench-threads.
my @backing = 0 xx 64;
my @slots := @backing;

for ^4 -> $t {
    for ^20000 -> $i {
        @slots[$t * 16 + ($i +& 15)] = $t * 1000 + $i;
    }
}
$checksum += [+] @slots;

say "checksum = $checksum";

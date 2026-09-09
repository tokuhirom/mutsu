# Concurrency - cross-thread traffic through ALIASED (celled) containers.
#
# Deliberately routes through `:=`-bound containers rather than plain lexicals.
# A plain `my @a` captured by a `start` block is served by the name-keyed
# shared-variable lanes; a bound one becomes a shared ContainerRef cell, which
# is the path ADR-0068's cross-thread container guard actually covers -
# `Value::with_deref` / `into_deref` on the read side, the element store on the
# write side. Drop the `:=` and this benchmark measures neither of them (issue
# #7613 measured that: the obvious plain-lexical shape takes zero guards).
#
# Threaded wall-clock on a shared CI runner is noisier than the single-threaded
# benchmarks here, and the worker count is fixed rather than derived from the
# runner: read the trend across commits, not a single row.

my $checksum = 0;

# Read-hot: four workers reading one shared cell. Every read of @alias and
# $sref goes through that cell, so this section is dominated by the read-side
# guard once a worker has been spawned.
my @data = ^256;
my @alias := @data;
my $seed = 7;
my $sref := $seed;

my @readers;
for ^4 {
    @readers.push: start {
        my $s = 0;
        for ^150000 -> $i {
            $s += @alias[$i +& 255] + $sref;
        }
        $s
    }
}
$checksum += [+] await @readers;

# Write side: disjoint slots of one aliased container. Each worker owns 16
# indices and writes them repeatedly, so the surviving values - and hence the
# checksum - do not depend on how the workers interleave.
my @backing = 0 xx 64;
my @slots := @backing;

my @writers;
for ^4 -> $t {
    @writers.push: start {
        for ^20000 -> $i {
            @slots[$t * 16 + ($i +& 15)] = $t * 1000 + $i;
        }
        1
    }
}
await @writers;
$checksum += [+] @slots;

say "checksum = $checksum";

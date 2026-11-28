# Indexed element assignment (issue #8086).
#
# Nothing else in benchmarks/ writes through a subscript: bench-array does
# push/map/grep/sort/reverse, array-ops does grep/map/elems, hash-access and
# bench-hash read. `@a[$i] = $v` and `%h{$k} = $v` are among the most common
# statements in the language and had no coverage at all, which is why #8069's
# ~46x gap on the array form stayed invisible until a threaded benchmark was
# profiled for an unrelated reason.
#
# Deliberately kept to the SIMPLEST shape the store path has: a plain
# non-negative Int / Str key, a plain rvalue, no type constraint, no default, no
# shape, no `:=`. That is the shape the fast path is supposed to serve, so any
# cost here is cost the fast path is failing to avoid.

sub workload() {
    my $checksum = 0;

    # Array: overwrite 16 slots of a 64-element array, 170000 times.
    my @arr = 0 xx 64;
    loop (my $i = 0; $i < 170000; $i++) {
        @arr[$i +& 15] = $i;
    }
    $checksum += @arr[0] + @arr[15];

    # Hash: the associative twin, over 16 pre-existing keys so the cost measured is
    # the store and not repeated growth.
    my %h;
    %h{$_} = 0 for ^16;
    loop (my $j = 0; $j < 70000; $j++) {
        %h{$j +& 15} = $j;
    }
    $checksum += %h<0> + %h<15>;

    # Nested: `@a[$i][$j] = v` takes the chained-subscript store, a different funnel
    # from the single-subscript one above (ADR-0068 §4 step 3).
    my @grid;
    @grid[$_] = [0 xx 8] for ^8;
    loop (my $k = 0; $k < 35000; $k++) {
        @grid[$k +& 7][($k +> 3) +& 7] = $k;
    }
    $checksum += @grid[0][0];

    "checksum = $checksum";
}

# Warm timed section (convention: benchmarks/bench-regex-match.raku, docs/benchmarks.md):
# the workload runs twice untimed and a third time timed, and that third run is printed
# as `bench-section-seconds:`, so scripts/bench-ci.sh records a `@section` series free of
# interpreter startup. Under scripts/bench-det.sh (BENCH_DET=1) it runs once.
my $warm = %*ENV<BENCH_DET> ?? 0 !! 2;
workload() for ^$warm;
my $t0 = now;
my $result = workload();
say "bench-section-seconds: {now - $t0}";
say $result;

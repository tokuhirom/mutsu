# Word-frequency counting: string-keyed hash increments over repeated text.
sub workload() {
    my @vocab = <the quick brown fox jumps over lazy dog and cat sat on mat
                 with some more words that repeat across every single line
                 counting frequencies is a classic small text processing task>;

    my @lines;
    for ^50 -> $i {
        my @ws;
        for ^20 -> $j {
            @ws.push(@vocab[($i * 7 + $j * 3) % @vocab.elems]);
        }
        @lines.push(@ws.join(' '));
    }

    my %counts;
    for ^100 {
        for @lines -> $line {
            for $line.split(' ') -> $w {
                %counts{$w}++;
            }
        }
    }
    "{%counts.elems} {[+] %counts.values}";
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

# Captures and Match access: what a successful match has to build and hand back.
#
# The matcher work here is deliberately cheap and the *result* is the point.
# Since ADR-0016 a Match is a span into a shared subject that materializes
# lazily, so the cost of a capturing match is split between recording spans
# during the match and materializing them when something reads them -- and only
# a benchmark that actually reads them can see the second half. Every branch
# below therefore consumes what it captured.
#
#   1. named captures, then $/.from / $/.to   (span reads, no Str needed)
#   2. positional captures coerced to Str and Int (forces materialization)
#   3. a quantified capture group, read through .elems and an index
#      (the list-valued capture shape: one node per repetition)
#   4. nested captures, read through the inner capture of an outer one
#   5. a capturing match that FAILS, so the spans recorded along the way are
#      all discarded -- the cost of bookkeeping that buys nothing
#
# WARM COST (#9916, ADR-0099 §6). The workload runs as `workload()` twice untimed
# and a third time timed, and that third run is printed as
# `bench-section-seconds:`, so scripts/bench-ci.sh records a `@section` series
# whose raku column is rakudo's settled, post-warm-up time. The whole-script
# series still exists but charges rakudo its warm-up (ADR-0099 §2.1 measured
# that as most of a 4.2x headline); the section ratio is the one that steers
# work against rakudo. Under scripts/bench-det.sh (BENCH_DET=1) the workload
# runs once, so the deterministic series keeps its old meaning.

sub workload() {
    my @lines;
    for ^40 -> $i {
        @lines.push("key{$i % 11}=value-{$i}; n={$i * 13 % 400}; words=the quick brown fox {$i}");
    }

    my $acc = 0;
    for ^30 {
        for @lines -> $l {
            if $l ~~ / $<key> = [ \w+ ] '=' $<val> = [ <[\w\-]>+ ] / {
                $acc += $<key>.chars + $<val>.chars;
                $acc += $/.from + $/.to;
            }
            if $l ~~ / (\w+) '=' (\d+) / {
                $acc += $0.Str.chars + $1.Int;
            }
            if $l ~~ / 'words=' [ (\w+) \s* ]+ / {
                $acc += $0.elems + $0[0].Str.chars;
            }
            if $l ~~ / ( 'key' (\d+) ) / {
                $acc += $0[0].Str.chars;
            }
            if $l ~~ / ( \w+ ) '=' ( \w+ ) '=' ( \w+ ) '=' /  {
                $acc += 1000000;
            }
        }
    }
    "regex-capture: acc=$acc";
}

my $warm = %*ENV<BENCH_DET> ?? 0 !! 2;
workload() for ^$warm;
my $t0 = now;
my $result = workload();
say "bench-section-seconds: {now - $t0}";
say $result;

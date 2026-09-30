# Repeated matching across a whole subject: :g, comb, subst.
#
# These share one mechanism the single-match benchmark never reaches -- resume-
# from-the-last-end scanning over a multi-kilobyte subject, producing one result
# per occurrence -- and differ in what they build from it:
#
#   .match(:g)   a list of Match objects (N materializations)
#   .comb(rx)    a list of Str                (no Match survives)
#   .subst(:g)   a rebuilt Str, replacement computed per occurrence
#   s:g///       the same, in place, through the assignment/container path
#   .subst with a closure replacement, which reads $0 per occurrence and so
#              forces that capture to materialize on every hit
#
# A regression in the scan loop moves all of them; a regression in Match
# construction or in the replacement path moves only its own line.
#
# The subject here is ~2.6 KB and each op runs many times. `split` deliberately
# lives in bench-regex-split-subst.raku instead: it is superlinear in subject
# length today, so at any interesting size it would dominate this file and mask
# everything else in it.
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
        @lines.push("user{$i % 13}=alpha-{$i} score={$i * 17 % 500} note=\"the quick brown fox {$i}\" flags=a,b,c");
    }
    my $text = @lines.join("\n");

    my $n = 0;
    for ^45 {
        $n += $text.match(/ \d+ /, :g).elems;
        $n += $text.comb(/ <[a..z]>+ /).elems;
        $n += $text.subst(/ 'score=' \d+ /, 'score=0', :g).chars;
        $n += $text.subst(/ 'user' (\d+) /, { 'u' ~ $0 }, :g).chars;
        my $copy = $text;
        $copy ~~ s:g/ <[0..9]>+ /#/;
        $n += $copy.chars;
    }
    "regex-global: n=$n";
}

my $warm = %*ENV<BENCH_DET> ?? 0 !! 2;
workload() for ^$warm;
my $t0 = now;
my $result = workload();
say "bench-section-seconds: {now - $t0}";
say $result;

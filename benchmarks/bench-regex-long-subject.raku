# Regex scanning over a LONG subject: per-position cost, measured once.
#
# Every other regex benchmark here runs small patterns over ~100-character
# lines many times, so what it measures is dominated by per-call setup. This one
# inverts that: one 128 KB subject, one call per pattern. What is left is the
# cost of *advancing one position and failing*, multiplied by the length of the
# subject -- and that is the quantity that decides whether mutsu keeps up on
# real input.
#
# It is the axis on which mutsu is currently weakest, and the suite was blind to
# it. Measured locally (release, 2026-09-13) on a subject of repeated 46-char
# units, one failing alternation scan, mutsu vs rakudo 2026.07:
#
#      40 KB   50 ms  vs  234 ms   0.21x
#     160 KB  167 ms  vs  331 ms   0.50x
#     640 KB  635 ms  vs  508 ms   1.25x
#
# mutsu is linear here and so is rakudo, but with a much larger constant: the
# apparent win at small sizes is rakudo's ~200 ms startup, and it is spent by
# ~0.5 MB. A change that lowers the per-position constant shows up here and
# almost nowhere else in the suite.
#
# 128 KB is chosen so the whole file costs a few hundred milliseconds today; the
# ratio against raku is expected to be unflattering compared to the short-line
# benchmarks, which is the point of keeping it.
#
# Update 2026-09-30 (#9916): the ADR-0099 Stage 1 prefilter turned this file
# around. Its warm section measures mutsu 8 ms against rakudo 204 ms at 128 KB,
# because rows 1-3 and 6-7 now reject positions without entering the engine.
# So there is no longer a crossover to size past. The walk's own per-position
# cost, which this file used to expose, is measured by
# bench-regex-scan-walk.raku, whose patterns no prefilter can help. This file is
# kept at its size so its history stays continuous.
#
#   1. failing literal scan        the cheapest possible per-position reject
#   2. failing alternation scan    four literals ranked at every position
#   3. failing :i literal scan     case folding on the reject path
#   4. failing greedy-run scan     \w+ grown and given back at every position
#   5. the same under :r           ratchet: grown once, no give-back
#   6. failing negative look-behind  an assertion evaluated per position
#   7. end anchor                  the engine must walk to the tail
#   8. succeeding scan near the end  the win case, for contrast
#   9. comb over the whole subject  the linear all-occurrences path
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
    my $unit = "abc def ghi jkl mno pqr stu vwx yz01 2345 67-8 \n";
    my $big  = $unit x (131072 div $unit.chars);

    my $acc = 0;
    $acc++ if $big ~~ / 'zzzq-not-here' /;
    $acc++ if $big ~~ / [ 'zzq' | 'yyq' | 'xxq' | 'wwq' ] /;
    $acc++ if $big ~~ / :i 'ZZZQ' /;
    $acc++ if $big ~~ / \w+ 'QQQ' /;
    $acc++ if $big ~~ / :r \w+ 'QQQ' /;
    $acc++ if $big ~~ / 'QQ' <!after \d > /;
    $acc++ if $big ~~ / '67-8' \s $ /;
    $acc++ if $big ~~ / 'yz01 2345' /;
    $acc += $big.comb(/ \d+ /).elems;
    "regex-long-subject: chars={$big.chars} acc=$acc";
}

my $warm = %*ENV<BENCH_DET> ?? 0 !! 2;
workload() for ^$warm;
my $t0 = now;
my $result = workload();
say "bench-section-seconds: {now - $t0}";
say $result;

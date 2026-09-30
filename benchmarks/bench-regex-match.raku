# Regex matching core: boolean `~~` over many short subjects.
#
# Deliberately capture-free and Match-free: every condition is used only for
# its truth value, so what this measures is the *matcher* -- candidate
# generation, the find loop that retries every start position, character-class
# and quantifier stepping, and LTM ranking of an alternation -- with none of
# the Match-object construction that bench-regex-capture.raku isolates.
#
# The dimensions, one per line of the inner loop:
#   1. literal + quantified class    'code=' \d+
#   2. anchored, bounded quantifier   ^ \d ** 4 ...
#   3. alternation (LTM ranking)      [ debug | info | warn | error | fatal ]
#   4. :i case folding                :i 'MSG='
#   5. a scan that CANNOT match       every start position tried and rejected
#   6. end anchor after a class run   <[a..z]>+ '-' \d+ $
#   7. a pattern interpolated from a Str variable
#   8. a pattern interpolated from a Regex variable (<$rx>)
#
# (5) matters as much as the successes: failing scans are what real code spends
# its time on, and they are the only case where the whole subject is walked.
# (7)/(8) exercise the interpolated-subpattern path, which has its own parse
# and cache behaviour that a literal regex never reaches.
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
    for ^60 -> $i {
        @lines.push("2026-09-{10 + $i % 20} 1{$i}:{$i % 60}:00 host{$i % 7} level=info code={$i * 37 % 900} msg=\"request {$i} handled in {$i * 3}ms\" tag=alpha-{$i % 5}");
    }

    my $needle = 'host';
    my $rx = / 'handled in' \s \d+ 'ms' /;

    my $hits = 0;
    for ^20 {
        for @lines -> $l {
            $hits++ if $l ~~ / 'code=' \d+ /;
            $hits++ if $l ~~ / ^ \d ** 4 '-' \d\d '-' \d\d /;
            $hits++ if $l ~~ / 'level=' [ 'debug' | 'info' | 'warn' | 'error' | 'fatal' ] /;
            $hits++ if $l ~~ / :i 'MSG=' /;
            $hits++ if $l ~~ / 'no-such-token-here' /;
            $hits++ if $l ~~ / <[a..z]>+ '-' \d+ $ /;
            $hits++ if $l ~~ / $needle \d+ /;
            $hits++ if $l ~~ / <$rx> /;
        }
    }
    "regex-match: hits=$hits";
}

my $warm = %*ENV<BENCH_DET> ?? 0 !! 2;
workload() for ^$warm;
my $t0 = now;
my $result = workload();
say "bench-section-seconds: {now - $t0}";
say $result;

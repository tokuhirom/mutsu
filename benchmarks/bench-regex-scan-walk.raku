# Unanchored scans that no prefilter can help: the tree walk's own cost (#9916).
#
# The Stage 1 prefilter (ADR-0099) answers "can a match start here?" from a
# required literal, a first-character set or a leading character chain, so
# `bench-regex-long-subject`'s failing literal and alternation scans now reject
# most positions without entering the engine at all. That is the right outcome,
# and it leaves the suite blind to what the engine costs once it IS entered.
# These patterns carry no required literal and a dense first set (`\w` admits
# most of an ordinary text), so nearly every position runs the matcher: what is
# timed is the per-position cost of the walk itself.
#
# They are the ADR-0135 §2.3 rows, the kill criterion of its Slice A (#10251):
# that slice must make this file's section at least 5x faster. Measured
# 2026-09-30 at 640 KB (release, best of 3): mutsu 370 / 1,350 ms, rakudo
# 385 / 1,320 ms, a flat backtracking prototype 11.7 / 33.4 ms. Both engines
# are linear in the subject, so 160 KB is used here to keep the file near
# half a second per run.
#
#   1. failing `\w+ \s \d ** 6`            grow a word, give it back, fail
#   2. failing `[ \w+ \s ] ** 3 \d ** 6`   the same, three levels of give-back
#   3. `(\w+) \s (\d+)` under :g           the success path, with captures
#
# WARM COST: two untimed runs, then a timed one printed as
# `bench-section-seconds:` -- see bench-regex-match.raku's header. Under
# scripts/bench-det.sh (BENCH_DET=1) the subject shrinks to 16 KB and the
# workload runs once.

my $unit = "the quick brown fox 12345 jumps over 67-8 lazy\n";
my $bytes = %*ENV<BENCH_DET> ?? 16384 !! 163840;
my $big  = $unit x ($bytes div $unit.chars);

sub workload() {
    my $acc = 0;
    $acc++ if $big ~~ / \w+ \s \d ** 6 /;
    $acc++ if $big ~~ / [ \w+ \s ] ** 3 \d ** 6 /;
    $acc += $big.match(/ (\w+) \s (\d+) /, :g).elems;
    "regex-scan-walk: chars={$big.chars} acc=$acc";
}

my $warm = %*ENV<BENCH_DET> ?? 0 !! 2;
workload() for ^$warm;
my $t0 = now;
my $result = workload();
say "bench-section-seconds: {now - $t0}";
say $result;

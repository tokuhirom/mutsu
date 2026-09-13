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
say "regex-match: hits=$hits";

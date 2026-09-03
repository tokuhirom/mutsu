use v6;
use Test;
use lib 't/lib';
use BacktraceFixture;

# `current_source_file_sym()` supplies the CALL-SITE file recorded on every
# `RoutineFrame` (ADR-0037 Slice 1 made all four call paths push one). It
# memoizes the `?FILE` -> Symbol intern on the identity of the `Arc<String>`
# the env returns, so a stale memo would pin whichever unit was seen first and
# mis-attribute every later frame.
#
# Alternate between a module sub and a script sub repeatedly: each round must
# still see BOTH files in the backtrace, in the right roles. A memo that failed
# to invalidate would report the same file for both after the first round.

plan 12;

sub script-dies() { die "script boom" }

for 1..3 -> $round {
    try fixture-dies();
    my $mod-bt = $!.backtrace;
    ok any($mod-bt>>.file) ~~ /BacktraceFixture\.rakumod/,
        "round $round: module frame reports the module file";
    ok any($mod-bt>>.file) ~~ /'callsite-file-alternating-units'\./,
        "round $round: the calling script file is still present";

    try script-dies();
    my $script-bt = $!.backtrace;
    ok any($script-bt>>.file) ~~ /'callsite-file-alternating-units'\./,
        "round $round: script-only backtrace reports the script file";
    nok any($script-bt>>.file) ~~ /BacktraceFixture\.rakumod/,
        "round $round: script-only backtrace does not leak the module file";
}

done-testing;

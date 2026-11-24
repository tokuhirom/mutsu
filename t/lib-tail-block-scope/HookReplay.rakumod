# A module that hands out a routine, a term and a variable through a
# `sub EXPORT` hook, and counts how often the hook runs (#19102).
module HookReplay {
    our $runs = 0;
}

sub EXPORT {
    $HookReplay::runs++;
    $PROCESS::hook-replay-runs++;
    my $stamp = $HookReplay::runs;
    my sub hooked($v) { "hooked:$v:$stamp" }
    Map.new(
        '&hooked' => &hooked,
        'hook-term' => "term:$stamp",
        '$hook-var' => 42,
    )
}

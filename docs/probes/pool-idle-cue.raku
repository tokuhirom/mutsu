# Idle-thread footprint: N long-period `cue(:every)` entries that never fire
# during the window. In mutsu each owns a thread for its whole lifetime
# (scheduler.rs -> scheduler_run_every_loop), and spawn_user_thread reserves
# USER_THREAD_STACK_SIZE = 256 MiB of address space apiece.
# Usage: <impl> docs/probes/pool-idle-cue.raku [N]
# 2026-07-17, release, 12 cores, N=50:
#   mutsu: RSS +20.7 MB, VmSize +16.4 GB (= 50 x 256 MiB), threads 2 -> 52
#   raku : RSS  +4.3 MB, VmSize   +25 MB,                  threads 2 -> 5
my $n = (@*ARGS[0] // 50).Int;

sub stats($label) {
    my %v;
    for "/proc/self/status".IO.lines -> $l {
        for <VmRSS VmSize Threads> -> $k {
            %v{$k} = $l.subst(/^ $k ':' \s* /, '') if $l.starts-with("$k:");
        }
    }
    say "$label: RSS={%v<VmRSS> // '?'} VmSize={%v<VmSize> // '?'} Threads={%v<Threads> // '?'}";
}

stats('before');
my @c = (^$n).map({ $*SCHEDULER.cue({ 1 }, :every(60)) });
sleep 1;
stats("after $n cue(:every(60))");

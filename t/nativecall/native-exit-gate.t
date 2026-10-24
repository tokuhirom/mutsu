use Test;

plan 3;

# #14864: process exit must not run libc's exit handlers (OpenSSL's
# OPENSSL_cleanup frees its global library context) while another thread is
# still inside a native call. The exit gate makes `exit` wait for in-flight
# native calls, so a thread inside a 240 ms `usleep` delays the process end
# until the call returns. This pins mutsu's guarantee (ADR-12814, guarantee 2:
# state the program cannot see stays sound), not a Raku-level behavior.

my $script = q:to/RAKU/;
    use NativeCall;
    sub usleep(uint32 --> int32) is native { * }
    start {
        usleep(1);               # warm the binding up
        usleep(240_000);         # still running when the main thread exits
    }
    sleep 0.1;
    say now.Rat;
    $*OUT.flush;
    exit 0;
    RAKU

my $proc = run($*EXECUTABLE, '-e', $script, :out, :err);
my $at-exit-call = $proc.out.slurp(:close).trim.Rat;
my $ended = now.Rat;

is $proc.exitcode, 0, 'the program exits with its own status';
ok $at-exit-call > 0, 'the child reported when it reached exit';
ok $ended - $at-exit-call >= 0.08,
    'exit waited for the thread that was inside a native call';

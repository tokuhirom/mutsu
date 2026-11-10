use Test;

# A native call that blocks in C (`accept`, `recv`, `connect`, `usleep`) counts
# its thread as blocked, as MoarVM marks a thread blocked around a native call:
# the worker pool grows past it for queued tasks (ADR-0123), and the thread
# counts quiescent for the cycle collector's stop-the-world. A `&callback` C
# calls back into Raku re-enters as a mutator.
#
# From TCP::LowLevel's t/01-basic-compat.t: each subtest leaves a `start`
# worker parked in a native `accept`/`recv`, and under the ecosystem sandbox's
# `ulimit -v` the pool's stack budget ran out after eleven of them. The pool
# still counted those workers as running, queued the next task for them, and
# the test hung. `MUTSU_STACK_BUDGET_MB=0` reproduces that without a ulimit.

plan 6;

my $exe = $*EXECUTABLE.absolute;

sub run-child(Str $code, *%env) {
    my %e = %*ENV;
    %e{.key} = .value for %env;
    my $p = run $exe, '-e', $code, :out, :err, :env(%e);
    my $out = $p.out.slurp(:close);
    my $err = $p.err.slurp(:close);
    ($p.exitcode, $out, $err)
}

# Ten workers sitting in a native `usleep`, then a task that must not wait for
# any of them to come back.
{
    my $code = q:to/CODE/;
        use NativeCall;
        sub usleep(uint32 --> int32) is native {*}
        my @blocked = (^10).map: { start { usleep(1_500_000); 'slept' } };
        sleep 0.2;
        my $p = start { 42 };
        await Promise.anyof($p, Promise.in(1));
        say $p.status;
        say (await @blocked).elems;
        CODE
    my ($exit, $out, $err) = run-child($code, MUTSU_STACK_BUDGET_MB => 0);
    is $out.lines.join(','), 'Kept,10',
        'a task queued behind workers blocked in native calls still runs';
    is $exit, 0, '... and the program exits cleanly';
}

# Callbacks into Raku from native code running on several workers at once,
# with the collector asked to run at every safepoint: each callback body runs
# as a mutator, so allocation inside it is safe.
{
    my $code = q:to/CODE/;
        use NativeCall;
        sub qsort(CArray[int32], size_t, size_t, &cmp (Pointer, Pointer --> int32))
            is native {*}
        sub deref(Pointer $p) { nativecast(CArray[int32], $p)[0] }
        my @p = (^4).map: -> $t {
            start {
                my @in = (^24).map({ ($_ * 7 + $t) % 24 });
                my $a = CArray[int32].new(@in);
                qsort($a, 24, 4, sub (Pointer $x, Pointer $y --> int32) {
                    my @garbage = [1, [2, 3]] xx 3;   # allocate inside the callback
                    deref($x) - deref($y)
                });
                (^24).map({ $a[$_] }).List eqv (^24).List
            }
        };
        say (await @p).all.so;
        CODE
    my ($exit, $out, $err) = run-child($code, MUTSU_GC_EVERY_SAFEPOINT => 1);
    is $out.trim, 'True', 'callbacks on several workers sort correctly under GC pressure';
    is $exit, 0, '... and the program exits cleanly';
    is $err, '', '... with nothing on stderr';
}

# A `whenever` body that makes a native call is still handling its event: the
# emitter keeps waiting for it (ADR-11268), as a Rakudo emitter waits on the
# react's lock. Net::BGP's t/90-basic-bgp.t double-closed a socket when the
# emitting `recv` loop ran on while the handler was inside a native `close`.
{
    use NativeCall;
    sub getpid(--> int32) is native {*}
    my @got;
    my $s = Supplier.new;
    my $ready = Promise.new;
    start react {
        whenever $s.Supply -> $m { getpid(); @got.push($m) }
        $ready.keep;
    }
    await $ready;
    $s.emit($_) for 1..3;
    is +@got, 3, 'an emit waits for a whenever body that makes a native call';
}

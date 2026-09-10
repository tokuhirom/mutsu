use Test;

# A supply block publishes its reactions in the order their sources produced
# them, even when a reaction is driven by a resolved Promise and therefore runs
# on a pooled worker rather than on the thread that resolved it (#7811).
#
# Before the fix the supply block's serialize lock was a barging condvar lock
# and the promise waiters were dispatched as N independent pooled tasks, so N
# nested `whenever <Promise>` bodies came out in whichever order N workers
# happened to wake up in. `10 30 20` was roughly a coin flip at three values.

plan 4;

# Three nested whenevers, one per emitted value, each on its own promise kept
# inside the outer whenever body. The keeps happen in a definite order on one
# thread, so the emits must reach the tap in that order.
sub run-nested(Int $n) {
    my $src = Supplier.new;
    my @fired;
    my $done = Promise.new;
    my $out = supply {
        whenever $src -> $v {
            my $p = Promise.new;
            whenever $p { emit $v * 10 }
            $p.keep;
        }
    };
    $out.tap({ @fired.push($_); $done.keep if @fired.elems == $n });
    $src.emit($_) for 1..$n;
    await Promise.anyof($done, Promise.in(10));
    @fired
}

is run-nested(3), [10, 20, 30], 'nested whenever-on-promise emits stay in keep order';

# The same at a length where several continuations are in flight at once, which
# is where a per-promise pooled task previously reordered even neighbours.
is run-nested(20), [(1..20).map(* * 10)],
        'the order holds with many continuations in flight at once';

# A promise kept BEFORE its nested `whenever` is registered runs the body
# synchronously on the registering thread; it must still land in sequence
# rather than jumping ahead of an earlier value's pending continuation.
#
# This alternating shape is the one that actually detects the #7831 hole, and
# the only one in this file that can: with the ticket reservation for the
# already-resolved path removed, it failed 18 of 20 release runs on a 4-core
# container (#7838), while tests 1, 2 and 4 stayed green every time. Do not
# fold it into test 4 -- see the note there.
my $src = Supplier.new;
my @mixed;
my $done = Promise.new;
my $out = supply {
    whenever $src -> $v {
        my $p = Promise.new;
        # Odd values resolve before the nested whenever sees the promise,
        # even values after it.
        $p.keep if $v %% 2;
        whenever $p { emit $v }
        $p.keep unless $v %% 2;
    }
};
$out.tap({ @mixed.push($_); $done.keep if @mixed.elems == 6 });
$src.emit($_) for 1..6;
await Promise.anyof($done, Promise.in(10));
is @mixed, [1, 2, 3, 4, 5, 6], 'already-kept and later-kept promises interleave in value order';

# Every promise kept before its nested `whenever` sees it, so every reaction
# takes the already-resolved path. That path used to run the body inline on the
# registering thread with no place in the block's sequencer at all (#7831), so
# it had to be ordered by luck; it must be ordered by the reservation instead.
#
# Note what this test can and cannot catch. It pins the reservation: if the
# tickets stopped ordering the pooled workers, ten reactions racing for them
# would come out shuffled. It cannot catch a *return* to running the body
# inline, because then every reaction runs on the registering thread in
# registration order and is trivially sorted -- which is why it stayed green
# through all 20 of the reverted runs noted on test 3. Test 3 is the pin;
# this one guards the mechanism that replaced the inline path.
my $src2 = Supplier.new;
my @kept-first;
my $done2 = Promise.new;
my $out2 = supply {
    whenever $src2 -> $v {
        my $p = Promise.new;
        $p.keep;
        whenever $p { emit $v }
    }
};
$out2.tap({ @kept-first.push($_); $done2.keep if @kept-first.elems == 10 });
$src2.emit($_) for 1..10;
await Promise.anyof($done2, Promise.in(10));
is @kept-first, [1 .. 10], 'promises kept before registration stay in value order';

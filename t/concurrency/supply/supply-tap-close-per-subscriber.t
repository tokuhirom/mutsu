use Test;

# Issue #9899: `Tap.close` on one tap of a channel-backed Supply (here
# `Supply.interval`) closes that tap only; the other taps keep receiving.

plan 4;

sub wait-until(&cond, :$timeout = 10) {
    my $deadline = now + $timeout;
    sleep 0.01 until cond() || now > $deadline;
    cond();
}

for <first second> -> $which {
    my $s = Supply.interval(0.02);
    my ($a, $b) = 0, 0;
    my $lock = Lock.new;
    my $t1 = $s.tap({ $lock.protect: { $a++ } });
    my $t2 = $s.tap({ $lock.protect: { $b++ } });
    ok wait-until({ $lock.protect: { $a >= 2 && $b >= 2 } }),
        "both taps receive before closing the $which";

    my ($closed, $open) = $which eq 'first' ?? ($t1, $t2) !! ($t2, $t1);
    $closed.close;
    my @at-close = $lock.protect: { [$a, $b] };
    my $open-idx = $which eq 'first' ?? 1 !! 0;
    my $still-open = wait-until({
        $lock.protect: { ($a, $b)[$open-idx] >= @at-close[$open-idx] + 5 }
    });
    $open.close;
    ok $still-open, "the other tap keeps receiving after the $which is closed";
}

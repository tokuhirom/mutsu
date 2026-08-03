use Test;

plan 5;

# A `whenever` written inside another `whenever`'s body only registers when
# that body runs -- which is inside the react event loop, long after the loop
# built its subscription set from the react body. The loop used to ignore such
# a subscription entirely: the outer `whenever` completing ended the react, and
# the inner one never fired.

{
    my $outer = Promise.new;
    my $s = Supplier.new;
    my @seen;
    start {
        sleep 0.2;
        $outer.keep(1);
        sleep 0.2;
        $s.emit(42);
        $s.done;
    }
    react {
        whenever $outer -> $v {
            @seen.push("outer:$v");
            whenever $s.Supply -> $x {
                @seen.push("inner:$x");
            }
        }
    }
    is @seen.join(' '), 'outer:1 inner:42',
            'a whenever registered from inside a whenever body runs';
}

{
    # The react must not finish while the nested subscription is still live.
    my $trigger = Promise.new;
    my $s = Supplier.new;
    my @seen;
    start {
        sleep 0.1;
        $trigger.keep;
        sleep 0.1;
        $s.emit($_) for 1, 2, 3;
        $s.done;
    }
    react {
        whenever $trigger {
            whenever $s.Supply -> $x { @seen.push($x) }
        }
    }
    is @seen.join(','), '1,2,3', 'the nested subscription keeps the react alive';
}

{
    # Two levels of nesting.
    my $a = Promise.new;
    my $b = Promise.new;
    my $c = Promise.new;
    my @seen;
    start { sleep 0.1; $a.keep('a'); sleep 0.1; $b.keep('b'); sleep 0.1; $c.keep('c') }
    react {
        whenever $a -> $x {
            @seen.push($x);
            whenever $b -> $y {
                @seen.push($y);
                whenever $c -> $z { @seen.push($z) }
            }
        }
    }
    is @seen.join(''), 'abc', 'whenevers nest to any depth';
}

{
    # `done` from a nested whenever ends the whole react.
    my $t = Promise.new;
    my $s = Supplier.new;
    my @seen;
    start { sleep 0.1; $t.keep; sleep 0.1; $s.emit($_) for 1, 2, 3 }
    react {
        whenever $t {
            whenever $s.Supply -> $x {
                @seen.push($x);
                done if $x == 2;
            }
        }
    }
    is @seen.join(','), '1,2', 'done inside a nested whenever ends the react';
}

{
    # A nested whenever on a Channel source is adopted too.
    my $t = Promise.new;
    my $ch = Channel.new;
    my @seen;
    start { sleep 0.1; $t.keep; sleep 0.1; $ch.send($_) for <x y>; $ch.close }
    react {
        whenever $t {
            whenever $ch -> $v { @seen.push($v) }
        }
    }
    is @seen.join(''), 'xy', 'a nested whenever on a Channel is adopted';
}

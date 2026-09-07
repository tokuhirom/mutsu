use Test;

plan 4;

# `Tap.close` on a `whenever` subscription stops FUTURE deliveries only.
#
# These two shapes originally asserted the opposite (that the close discarded
# whatever the react loop had not got to yet). Measured against rakudo
# v2026.07 both of those expectations are wrong: delivery inside a `react` is
# deferred to the drive loop's pump, but the close is ordered against the
# *emit*, not against the pump, so everything emitted while the tap was live
# still arrives.

# A Tap returned by `do whenever` can close its own subscription while its
# callback is running. Values emitted BEFORE that close still arrive.
{
    my $supplier = Supplier.new;
    my $seen = 0;
    react {
        my $tap;
        $tap = do whenever $supplier.Supply -> $value {
            $seen++;
            $tap.close;
        }
        $supplier.emit(1);
        $supplier.emit(2);
        $supplier.emit(3);
        whenever Promise.in(0.05) {
            is $seen, 3, 'a self-close delivers everything emitted before it';
            done;
        }
    }
}

# ... and nothing emitted after it.
{
    my $supplier = Supplier.new;
    my $seen = 0;
    react {
        my $tap;
        $tap = do whenever $supplier.Supply -> $value {
            $seen++;
            $tap.close;
        }
        $supplier.emit(1);
        whenever Promise.in(0.05) {
            $supplier.emit(2);
            $supplier.emit(3);
        }
        whenever Promise.in(0.2) {
            is $seen, 1, 'a self-close stops emissions made after it';
            done;
        }
    }
}

# A sibling whenever closing another subscription is ordered the same way: the
# `$left.emit('late')` runs before the right-hand callback (and so before the
# `.close`), so it is still delivered.
{
    my $left = Supplier.new;
    my $right = Supplier.new;
    my $left-seen = 0;
    react {
        my $left-tap = do whenever $left.Supply -> $value { $left-seen++ }
        my $right-tap = do whenever $right.Supply -> $value { $left-tap.close }
        $right.emit('stop');
        $left.emit('late');
        whenever Promise.in(0.05) {
            is $left-seen, 1, 'a sibling close does not un-emit an earlier value';
            done;
        }
    }
}

# The sibling close does shut the subscription down for later emissions --
# the form used to stop a listener from a signal subscription.
{
    my $left = Supplier.new;
    my $right = Supplier.new;
    my $left-seen = 0;
    react {
        my $left-tap = do whenever $left.Supply -> $value { $left-seen++ }
        my $right-tap = do whenever $right.Supply -> $value { $left-tap.close }
        $right.emit('stop');
        whenever Promise.in(0.05) { $left.emit('late') }
        whenever Promise.in(0.2) {
            is $left-seen, 0, 'a sibling whenever can close another Tap';
            done;
        }
    }
}

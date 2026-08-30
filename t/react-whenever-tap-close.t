use Test;

plan 2;

# A Tap returned by `do whenever` can close its own subscription while its
# callback is running. Queued later emissions must observe the closed id.
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
            is $seen, 1, 'closing a whenever Tap stops queued later emissions';
            done;
        }
    }
}

# A sibling whenever can close another subscription before it consumes an
# event, the form used to shut down a listener from a signal subscription.
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
            is $left-seen, 0, 'a sibling whenever can close another Tap';
            done;
        }
    }
}

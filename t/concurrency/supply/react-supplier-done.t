use v6;
use Test;

plan 4;

# Push-based supplier event delivery: values emitted (and the done) from
# another thread must all reach a `react whenever` and end the react, even
# when `Supplier.done`'s internal state reset races the drive loop. The old
# snapshot-polling loop lost the trailing emit + done here (delivered only
# the first value, then hung until timeout, busy-spinning a core).
{
    my $s = Supplier.new;
    my $sup = $s.Supply;
    my @got;
    start { sleep 0.2; $s.emit(1); $s.emit(2); $s.done; }
    react {
        whenever $sup {
            @got.push($_);
        }
    }
    is @got, [1, 2], 'both cross-thread emits delivered to the whenever';
}

# Same shape with the Supply created inline in the whenever.
{
    my $s = Supplier.new;
    my @got;
    start { sleep 0.2; $s.emit(42); $s.done; }
    react {
        whenever $s.Supply {
            @got.push($_);
        }
    }
    is @got, [42], 'inline .Supply whenever sees the emit and ends on done';
}

# A value emitted inside the react body itself (after the whenever) is
# delivered: the whenever's tap is already active. In mutsu the sink is
# registered when the drive loop starts (after the body ran), so this relies
# on registration replaying the values buffered in between.
{
    my $s = Supplier.new;
    my $sup = $s.Supply;
    my @got;
    react {
        whenever $sup {
            @got.push($_);
            done if $_ eq 'last';
        }
        $s.emit('body');
        $s.emit('last');
    }
    is @got, ['body', 'last'], 'emits from the react body reach the whenever';
}

# A cross-thread quit reaches the whenever's QUIT handler.
{
    my $s = Supplier.new;
    my $sup = $s.Supply;
    my $quit-message = '';
    start { sleep 0.2; $s.emit(7); $s.quit('boom'); }
    react {
        whenever $sup {
            QUIT { default { $quit-message = .message; done } }
        }
    }
    is $quit-message, 'boom', 'cross-thread quit delivered to the QUIT handler';
}

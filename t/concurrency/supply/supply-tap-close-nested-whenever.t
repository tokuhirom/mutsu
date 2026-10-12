use Test;

# Closing a tap on a `supply { }` block closes every subscription the block
# opened, including those opened from inside a `whenever` body, so their CLOSE
# phasers run before the block's own (#12864).

plan 5;

{
    my @log;
    my $s = Supplier.new;
    my $outer = supply {
        whenever $s.Supply -> $x {
            whenever supply { CLOSE @log.push("inner $x"); whenever Supplier.new.Supply { } } { }
        }
        CLOSE @log.push("outer");
    }
    my $t = $outer.tap;
    $s.emit(1);
    $s.emit(2);
    $t.close;
    # Rakudo promises no order among the nested taps themselves.
    is-deeply @log.head(2).sort.List, ("inner 1", "inner 2"),
        'closing the tap closes every nested whenever tap';
    is @log.tail, "outer", 'the block\'s own CLOSE runs after them';
}

{
    # A nested subscription whose source already finished is not closed again.
    my @log;
    my $s = Supplier.new;
    my $inner-src = Supplier.new;
    my $outer = supply {
        whenever $s.Supply -> $x {
            whenever supply { CLOSE @log.push("inner"); whenever $inner-src.Supply { } } { }
        }
        CLOSE @log.push("outer");
    }
    my $t = $outer.tap;
    $s.emit(1);
    $inner-src.done;
    is-deeply @log, ["inner"], 'a finished nested supply ran its CLOSE once';
    $t.close;
    is-deeply @log, ["inner", "outer"], 'closing the block does not re-run it';
}

{
    # The block completing with `done` closes its nested subscriptions too.
    my @log;
    my $s = Supplier.new;
    my $outer = supply {
        whenever $s.Supply -> $x {
            if $x eq 'stop' { done }
            else {
                whenever supply { CLOSE @log.push("inner"); whenever Supplier.new.Supply { } } { }
            }
        }
        CLOSE @log.push("outer");
    }
    $outer.tap;
    $s.emit('a');
    $s.emit('stop');
    is-deeply @log, ["inner", "outer"], '`done` in the block closes nested whenever taps';
}

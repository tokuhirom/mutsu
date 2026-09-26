use Test;

# `whenever $obj` coerces its source with `$obj.Supply`, so an object that
# declares its own `Supply` method (Timer::Stopwatch, a monitor exposing its
# supplier) is subscribed to, rather than emitted once as a value.

plan 3;

class Ticker {
    has $.supplier = Supplier.new;
    method Supply { $!supplier.Supply }
}

{
    my $t = Ticker.new;
    my @got;
    start { sleep .2; $t.supplier.emit($_) for 1, 2; $t.supplier.done }
    react {
        whenever $t { @got.push: $_ }
        whenever Promise.in(10) { done }
    }
    is-deeply @got, [1, 2], 'react whenever on an object with a Supply method';
}

{
    my $t = Ticker.new;
    my $s = supply { whenever $t { emit $_ * 10 } };
    my @got;
    start { sleep .2; $t.supplier.emit(3); $t.supplier.done }
    react {
        whenever $s { @got.push: $_ }
        whenever Promise.in(10) { done }
    }
    is-deeply @got, [30], 'supply-block whenever on an object with a Supply method';
}

{
    my $supplier = Supplier.new;
    my @got;
    start { sleep .2; $supplier.emit('s'); $supplier.done }
    react {
        whenever $supplier { @got.push: $_ }
        whenever Promise.in(10) { done }
    }
    is-deeply @got, ['s'], 'a Supplier is still coerced';
}

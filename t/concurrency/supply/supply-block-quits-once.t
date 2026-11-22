use Test;

# A `supply { }` block terminates exactly once (#18601). When one of its
# `whenever` sources quits, a later failure of another `whenever` -- a
# source whose tap fails (`Supply.interval` on a CurrentThreadScheduler) or a
# `whenever` body that dies -- must not reach the tap's `quit` handler again.
# EventSource::Server's `t/040-stop.t` kept a Promise from that handler and
# died with "already vowed". Also pins the Tinky shape: `Supply.merge` of an
# array of live `map` stages.

plan 3;

{
    my $*SCHEDULER = CurrentThreadScheduler.new;
    my $sup = Supplier.new;
    my $s = supply {
        whenever Supply.merge($sup.Supply, (^3).Supply).map({ die "boom" }) -> $m { emit $m }
        whenever Supply.interval(60) { }
    }
    my $p = Promise.new;
    my $quits = 0;
    $s.act(-> $ { }, quit => { $quits++; $p.keep });
    is $quits, 1, 'a block whose whenevers fail twice quits once';
}

{
    my $sup = Supplier.new;
    my $s = supply {
        whenever $sup.Supply.map({ die "boom" }) -> $m { emit $m }
        whenever Supply.from-list(1) { die "first" }
    }
    my @quits;
    $s.act(-> $ { }, quit => { @quits.push: .message });
    $sup.emit(1);
    is-deeply @quits, ['first'], 'a source failing after the block quit is not reported';
}

{
    my @suppliers = Supplier.new xx 3;
    my @supplies = @suppliers.kv.map(-> $i, $s { $s.Supply.map(-> $v { $i, $v }) });
    my @got;
    Supply.merge(@supplies).tap({ @got.push: $_ });
    @suppliers[$_].emit("x$_") for 2, 0, 1;
    is-deeply @got, [(2, 'x2'), (0, 'x0'), (1, 'x1')], 'Supply.merge of an array of live map stages';
}

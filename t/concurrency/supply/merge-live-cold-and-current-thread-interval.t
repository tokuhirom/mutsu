use Test;

# From the EventSource::Server distribution (t/020-basic.t, t/040-stop.t).

plan 8;

# A merge of a live Supplier and a cold supply keeps the cold values when it
# is transformed or used as a `whenever` source.
{
    my $sp = Supplier.new;
    my $data = <1 2 3>.Supply;
    my @got;
    Supply.merge($sp.Supply, $data).map({ $_ ~ "!" }).tap({ @got.push: $_ });
    is @got.join(","), "1!,2!,3!", "merge(live, cold).map keeps the cold values";

    my @w;
    supply { whenever Supply.merge($sp.Supply, $data) -> $m { emit $m } }.tap({ @w.push: $_ });
    is @w.join(","), "1,2,3", "whenever on merge(live, cold) delivers the cold values";
}

# Supply.interval under a CurrentThreadScheduler cannot honour :every; the
# enclosing supply quits with Rakudo's message.
{
    my $*SCHEDULER = CurrentThreadScheduler.new;
    my $pc = Promise.new;
    my $out = supply {
        whenever (^3).Supply -> $m { emit $m }
        whenever $pc { done }
        whenever Supply.interval(60) { }
    }
    my $quit;
    $out.act( -> $ { }, quit => { $quit = $_ });
    ok $quit.defined, "quit handler ran";
    isa-ok $quit, X::AdHoc, "quit reason is an exception";
    is $quit.message, "Cannot specify :every in CurrentThreadScheduler", "quit message";

    throws-like { $*SCHEDULER.cue({ }, :every(1)) }, X::AdHoc,
        message => "Cannot specify :every in CurrentThreadScheduler";
}

# The same distribution's stop test shape.
{
    my $*SCHEDULER = CurrentThreadScheduler.new;
    my $pc = Promise.new;
    my $out = supply {
        whenever (^10).Supply -> $m { emit $m }
        whenever $pc { done }
        whenever Supply.interval(60) { }
    }
    my $p = Promise.new;
    $out.act( -> $ { }, quit => { $p.keep });
    $pc.keep;
    await Promise.anyof($p, Promise.in(5));
    is $p.status, Kept, "quit called on the out supply";
}

# Plain merge.tap still works.
{
    my $sp = Supplier.new;
    my @got;
    Supply.merge($sp.Supply, <a b>.Supply).tap({ @got.push: $_ });
    $sp.emit("c");
    is @got.join(","), "a,b,c", "merge tap sees cold then live values";
}

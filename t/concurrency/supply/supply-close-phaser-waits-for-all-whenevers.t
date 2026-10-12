use Test;

# A supply block's CLOSE phaser runs once, when the block ends -- not as soon
# as one of several live `whenever` sources finishes (#12880).

plan 4;

{
    my @log;
    my $s = Supplier.new;
    my $t = supply {
        whenever Promise.in(0.05) { }
        whenever $s.Supply { }
        CLOSE @log.push: 'CLOSE';
    }.tap;
    sleep 0.3;
    @log.push: 'before close';
    $t.close;
    is-deeply @log, ['before close', 'CLOSE'], 'one finished source does not close the block';
}

{
    my @log;
    my $s = Supplier.new;
    supply {
        whenever Promise.in(0.05) { }
        whenever $s.Supply { }
        CLOSE @log.push: 'CLOSE';
    }.tap(done => { @log.push: 'done' });
    sleep 0.3;
    @log.push: 'before done';
    $s.done;
    sleep 0.1;
    is-deeply @log, ['before done', 'done', 'CLOSE'],
        'the last source finishing ends the block: done, then CLOSE';
}

{
    my @log;
    my $s = Supplier.new;
    supply { whenever $s.Supply { }; CLOSE @log.push: 'CLOSE' }.tap;
    $s.done;
    sleep 0.1;
    is-deeply @log, ['CLOSE'], 'a tap without done => still runs CLOSE at the end';
}

{
    my @log;
    my $s = Supplier.new;
    my $t = supply { whenever $s.Supply { }; CLOSE @log.push: 'CLOSE' }.tap;
    $s.done;
    sleep 0.1;
    $t.close;
    is-deeply @log, ['CLOSE'], 'a later Tap.close does not run CLOSE again';
}

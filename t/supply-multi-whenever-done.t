use Test;
plan 9;

# When a `supply` block with multiple `whenever`s completes via `done`, every
# whenever source's on-close callback fires and the downstream done runs once.
# See roast/S17-supply/syntax.t.

# Explicit `done` in the block body completes and closes all sources.
{
    my ($c1, $c2, $done) = False xx 3;
    my $t1 = Supplier.new;
    my $t2 = Supplier.new;
    my $s = supply {
        whenever $t1.Supply.on-close({ $c1 = True; }) { }
        whenever $t2.Supply.on-close({ $c2 = True; }) { }
        done;
    }
    $s.tap(done => { $done = True });
    ok $done, 'explicit done in body runs the downstream done';
    ok $c1, 'first whenever source closed';
    ok $c2, 'second whenever source closed';
}

# `done` inside one whenever body completes the whole supply (take-until).
{
    my ($c1, $c2, $done) = False xx 3;
    my $t1 = Supplier.new;
    my $t2 = Supplier.new;
    my $s = supply {
        whenever $t1.Supply.on-close({ $c1 = True; }) { emit $_; }
        whenever $t2.Supply.on-close({ $c2 = True; }) { done; }
    }
    my @collected;
    $s.tap({ @collected.push($_) }, done => { $done = True });
    $t1.emit('tea');
    $t1.emit('coffee');
    $t2.emit('beer');
    $t1.emit('cocoa');
    is @collected, ['tea', 'coffee'], 'take-until emitted the right values';
    ok $done, 'take-until done after the second source emits';
    ok $c1, 'first source closed on completion';
    ok $c2, 'second source closed on completion';
}

# Two whenevers with no explicit done: the supply is done only after ALL
# sources are done.
{
    my $done = False;
    my $t1 = Supplier.new;
    my $t2 = Supplier.new;
    my $s = supply { whenever $t1 { } whenever $t2 { } }
    $s.tap(done => { $done = True });
    $t2.done;
    nok $done, 'not done after only one source is done';
    $t1.done;
    ok $done, 'done after all sources are done';
}

use Test;

# `done` inside a `whenever` body of a `supply { ... }` block completes the
# supply: the source is not read further, the block's other `whenever`s are
# not subscribed, the whenever's own LAST phaser does not run (the block closes
# its subscriptions, it does not see them finish) and the tap's `done` callback
# fires once. A bare `done` in the block body and `done` in a `react` already
# worked and are kept as controls. Expected values come from `raku`.

plan 14;

# The reported case.
{
    my @got;
    my $s = supply { whenever Supply.from-list(1, 2, 3) { emit $_; done if $_ == 2 } };
    $s.tap({ @got.push: $_ });
    is @got.join(','), '1,2', 'done in a whenever stops the supply';
}

# The body is not run for the values after the `done`, LAST does not run, and
# the tap's done callback fires exactly once.
{
    my @log;
    my @got;
    my $s = supply {
        whenever Supply.from-list(1, 2, 3) {
            @log.push: "body$_";
            emit $_;
            done if $_ == 2;
            @log.push: "after$_";
            LAST { @log.push: 'LAST' }
        }
    };
    $s.tap({ @got.push: $_ }, done => { @log.push: 'DONE' });
    is @got.join(','), '1,2', 'only the values before the done are emitted';
    is @log.join(','), 'body1,after1,body2,DONE',
        'the body stops at done, LAST does not run, the tap sees done once';
}

# A done in the first of two whenevers closes the second as well.
{
    my @got;
    my $s = supply {
        whenever Supply.from-list(1, 2, 3) { emit "a$_"; done if $_ == 2 }
        whenever Supply.from-list(7, 8, 9) { emit "b$_" }
    };
    $s.tap({ @got.push: $_ });
    is @got.join(','), 'a1,a2', "a sibling whenever is not subscribed after the done";
}

# Without a done everything still flows and LAST still runs.
{
    my @log;
    my @got;
    my $s = supply {
        whenever Supply.from-list(1, 2, 3) {
            emit $_;
            LAST { @log.push: 'LAST' }
        }
    };
    $s.tap({ @got.push: $_ }, done => { @log.push: 'DONE' });
    is @got.join(','), '1,2,3', 'without a done every value is emitted';
    is @log.join(','), 'LAST,DONE', 'without a done LAST then the tap done run';
}

# A done on the very first value.
{
    my @got;
    supply { whenever Supply.from-list(1, 2, 3) { done; emit $_ } }.tap({ @got.push: $_ });
    is @got.elems, 0, 'a done on the first value emits nothing';
}

# The other ways a supply is read.
{
    my $s = supply { whenever Supply.from-list(1, 2, 3) { emit $_; done if $_ == 2 } };
    is $s.list.join(','), '1,2', '.list stops at the done';
    my $m = supply { whenever Supply.from-list(1, 2, 3, 4) { emit $_ * 10; done if $_ == 3 } };
    is $m.map(* + 1).list.join(','), '11,21,31', 'a derived supply stops at the done';
    my $p = supply { whenever Supply.from-list(1, 2, 3) { emit $_; done if $_ == 2 } }.Promise;
    is await($p), 2, '.Promise is kept with the last value before the done';
}

# Controls that already worked.
{
    my @got;
    supply { emit 1; emit 2; done; emit 3 }.tap({ @got.push: $_ });
    is @got.join(','), '1,2', 'a bare done in the supply body';
}
{
    my @r;
    react { whenever Supply.from-list(1, 2, 3) { @r.push: $_; done if $_ == 2 } };
    is @r.join(','), '1,2', 'done in a react whenever';
}
{
    my $source = Supplier.new;
    my @got;
    supply { whenever $source.Supply { emit $_; done if $_ == 2 } }.tap({ @got.push: $_ });
    $source.emit($_) for 1..4;
    is @got.join(','), '1,2', 'a live Supplier source stops at the done';
}
{
    my @got;
    my $done = 0;
    my $s = supply { whenever Supply.from-list(1, 2, 3) { emit $_; done if $_ == 2 } };
    $s.tap({ @got.push: $_ }, done => { $done++ });
    is $done, 1, 'the tap done callback fires once';
}

use v6;
use Test;

plan 12;

# A `supply { whenever <cold source> { ... } }` block driven OUTSIDE a react
# loop (.tap / .list / .wait) must run the whenever body and deliver its
# emissions. Previously the body's `emit` had no registered tap outside react
# and the values were silently dropped (tap), or the raw whenever marker
# leaked / returned empty (.list / .wait).

# --- .tap drives cold whenever sources ---
{
    my $sup = supply { whenever Supply.from-list(1, 2, 3) { emit $_ * 10 } };
    my @got;
    my $done = 0;
    $sup.tap({ @got.push($_) }, done => { $done = 1 });
    is @got.join(','), '10,20,30', 'tap on cold-whenever supply delivers emissions';
    is $done, 1, 'tap done callback fires after cold whenever completes';
}

# --- body-level emits and whenever emissions keep source order ---
{
    my $sup = supply {
        emit 0;
        whenever Supply.from-list(1, 2) { emit $_ }
    };
    my @got;
    $sup.tap({ @got.push($_) });
    is @got.join(','), '0,1,2', 'plain emits and cold whenever emissions stay in order';
}

# --- .list / .wait materialize the block ---
{
    my $sup = supply { whenever Supply.from-list(1, 2, 3) { emit $_ * 10 } };
    is $sup.list.join(','), '10,20,30', '.list materializes a cold-whenever supply';
    is $sup.wait, 30, '.wait returns the last emitted value';
}

# --- a captured lexical written by the whenever body is visible after ---
{
    my $count = 0;
    my $sup = supply {
        whenever Supply.from-list(1, 2, 3) { $count++; emit $_ }
    };
    $sup.wait;
    is $count, 3, 'whenever body writes to a captured lexical are visible after .wait';
}

# --- a lexical the caller reassigns after creation is seen by the body ---
{
    my $gate = 0;
    my $sup = supply { whenever Supply.from-list(1) { emit $gate } };
    $gate = 9;
    my @got;
    $sup.tap({ @got.push($_) });
    is @got.join(','), '9', 'supply body reads the current lexical value at tap time';
}

# --- LAST phaser of a cold whenever runs (and may emit) ---
{
    my $sup = supply {
        whenever Supply.from-list(1, 2) { emit $_; LAST { emit 99 } }
    };
    is $sup.list.join(','), '1,2,99', 'LAST phaser of a cold whenever runs and emits';
}

# --- die inside the body routes to the tap quit handler ---
{
    my $sup = supply { whenever Supply.from-list(1) { die "boom" } };
    my $quit = '';
    $sup.tap({ ; }, quit => { $quit = ~$_ });
    ok $quit.contains('boom'), 'die in a cold whenever body reaches the quit handler';
}

# --- Supply.do fires on live emissions ---
{
    my $s = Supplier.new;
    my $seen = 0;
    my @through;
    $s.Supply.do({ $seen++ }).tap({ @through.push($_) });
    $s.emit(7);
    $s.emit(8);
    is $seen, 2, 'Supply.do callback fires per live emission';
    is @through.join(','), '7,8', 'Supply.do passes live values through unchanged';
}

# --- on-close fires when the tap is closed ---
{
    my $s = Supplier.new;
    my $closed = 0;
    my $tap = $s.Supply.on-close({ $closed = 1 }).tap({ ; });
    $tap.close;
    is $closed, 1, 'on-close callback fires on Tap.close';
}

done-testing;

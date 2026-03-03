use Test;

plan 16;

# Basic wrap/unwrap
{
    my @log;
    sub foo { push @log, "foo" }
    sub wrapper { push @log, "before"; callwith(); push @log, "after" }

    foo();
    is +@log, 1, "one event logged";
    is @log[0], "foo", "it's foo";

    @log = ();
    my $h = &foo.wrap(&wrapper);
    foo();
    is @log.join('|'), 'before|foo|after', 'wrap works';

    @log = ();
    &foo.unwrap($h);
    foo();
    is +@log, 1, "unwrap restores original";
    is @log[0], "foo", "original function runs";
}

# Double wrapping
{
    my @log;
    sub bar { push @log, "bar" }
    sub w1 { push @log, "w1"; callwith(); }
    sub w2 { push @log, "w2"; callwith(); }

    my $h1 = &bar.wrap(&w1);
    my $h2 = &bar.wrap(&w2);
    bar();
    is @log.join('|'), 'w2|w1|bar', 'double wrap works';

    @log = ();
    &bar.unwrap($h2);
    bar();
    is @log.join('|'), 'w1|bar', 'unwrap outer leaves inner';

    @log = ();
    &bar.unwrap($h1);
    bar();
    is @log.join('|'), 'bar', 'fully unwrapped';
}

# Out-of-order unwrap
{
    my @log;
    sub baz { push @log, "baz" }
    sub wa { push @log, "wa"; callwith(); }
    sub wb { push @log, "wb"; callwith(); }

    my $h1 = &baz.wrap(&wa);
    my $h2 = &baz.wrap(&wb);
    &baz.unwrap($h1);  # unwrap inner first
    baz();
    is @log.join('|'), 'wb|baz', 'out-of-order unwrap works';
}

# .restore on WrapHandle
{
    sub qux { 42 }
    my $h = &qux.wrap(-> { 1 + callsame });
    is qux(), 43, 'wrap with callsame works';
    $h.restore();
    is qux(), 42, '.restore unwraps';
}

# callwith passes arguments through
{
    sub add($a, $b) { $a + $b }
    &add.wrap(sub (*@args) { 10 + callwith(|@args) });
    is add(1, 2), 13, 'callwith passes args through wrapper';
}

# dies-ok on never-wrapped sub
{
    sub virgin { 1 }
    dies-ok { &virgin.unwrap() }, 'cannot unwrap a never-wrapped sub';
}

# dies-ok on already-unwrapped handle
{
    sub target { 1 }
    my $h = &target.wrap(-> { callsame });
    &target.unwrap($h);
    dies-ok { &target.unwrap($h) }, 'cannot re-unwrap same handle';
}

# wrap with return value modification
{
    sub greeting { "hello" }
    &greeting.wrap(sub { callsame() ~ " world" });
    is greeting(), "hello world", "wrapper can modify return value";
}

# Multiple levels of wrapping
{
    sub levelwrap($n) { $n }
    for (1..3) -> $i {
        &levelwrap.wrap({ callwith($^t + 1) });
    }
    is levelwrap(1), 4, "multiple wraps accumulate";
}

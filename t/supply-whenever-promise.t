use Test;

plan 6;

# `whenever <Promise>` inside a `supply` block used to hand the tap the raw
# 4-element subscription marker instead of running the body: the marker/value
# separators recognised only a Supply source, so a Promise source fell through
# as an ordinary emitted value.

{
    my $s = supply { whenever Promise.in(0.05) { emit 'badger' } }
    my @got;
    $s.tap: { @got.push($_) };
    is @got, [], 'nothing before the promise resolves';
    sleep 0.3;
    is @got, ['badger'], 'the whenever body ran and its emit reached the tap';
}

{
    my $s = supply {
        whenever Promise.in(0.05) { emit 1 }
        whenever Promise.in(0.10) { emit 2 }
    }
    my @got;
    my $done = False;
    $s.tap: { @got.push($_) }, done => { $done = True };
    sleep 0.4;
    is @got, [1, 2], 'several promise sources each fire once, in time order';
    ok $done, 'the supply completes when every promise source is done';
}

{
    # The promise's result is the whenever body's topic.
    my $p = Promise.new;
    my $s = supply { whenever $p -> $v { emit $v * 2 } }
    my @got;
    $s.tap: { @got.push($_) };
    $p.keep(21);
    sleep 0.2;
    is @got, [42], 'the body sees the kept value as its parameter';
}

{
    # A broken promise quits the supply.
    my $p = Promise.new;
    $p.break('boom');
    my $s = supply { whenever $p { emit 'never' } }
    my @got;
    my $quit;
    $s.tap: { @got.push($_) }, quit => { $quit = $_ };
    sleep 0.2;
    is $quit.Str, 'boom', 'a broken promise source quits the tap';
}

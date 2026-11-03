use Test;

# An exception thrown by a downstream tap while a `supply` block's `whenever`
# body is emitting belongs to the consumer: it unwinds to whoever drove the
# emission and neither quits the block nor calls the consumer's `quit`
# handler, so the next event is still delivered.
# Found via IO::Socket::Async::SSL t/client-server.rakutest, where the first
# server-side tap body died and the second connection was never handed on.

plan 8;

{
    my $src = Supplier.new;
    my $s = supply { whenever $src.Supply -> $n { emit $n } };
    my $seen = 0;
    my $quits = 0;
    $s.tap: -> $v { $seen++; die "boom $v" }, quit => { $quits++ };
    my @thrown;
    for 1..2 -> $i {
        try { $src.emit($i); CATCH { default { @thrown.push: .message } } }
    }
    is $seen, 2, 'supply block keeps delivering after the tap died';
    is-deeply @thrown, ['boom 1', 'boom 2'], 'each error reaches the source emit';
    is $quits, 0, "the consumer's quit handler is not called";
}

{
    # A nested whenever fed by a kept promise, as IO::Socket::Async::SSL.listen does.
    my $src = Supplier.new;
    my $s = supply {
        whenever $src.Supply -> $n {
            my $p = Promise.kept($n);
            whenever $p -> $r { emit $r }
        }
    };
    my $seen = 0;
    $s.tap: -> $v { $seen++; die "z" };
    try { $src.emit(1) };
    sleep 0.1;
    try { $src.emit(2) };
    sleep 0.1;
    is $seen, 2, 'a nested whenever block survives a dying tap';
}

{
    # A die in the whenever body itself still quits the block.
    my $src = Supplier.new;
    my $s = supply { whenever $src.Supply -> $n { die "own $n" } };
    my $reason;
    $s.tap: -> $v { }, quit => { $reason = .message };
    try { $src.emit(1) };
    is $reason, 'own 1', 'an error raised by the body itself still quits the block';
}

{
    # A plain Supplier tap that dies does not stop the supplier either.
    my $sup = Supplier.new;
    my $c = 0;
    $sup.Supply.tap: -> $v { $c++; die "x" };
    try { $sup.emit(1) };
    try { $sup.emit(2) };
    is $c, 2, 'a plain Supplier keeps delivering after a tap died';
}

{
    # QUIT inside the whenever does not intercept the consumer's error either.
    my $src = Supplier.new;
    my $s = supply {
        whenever $src.Supply -> $n {
            emit $n;
            QUIT { default { } }
        }
    };
    my $seen = 0;
    $s.tap: -> $v { $seen++; die "q" };
    try { $src.emit(1) };
    try { $src.emit(2) };
    is $seen, 2, 'a QUIT phaser block does not end the supply over a consumer error';
}

{
    my $src = Supplier.new;
    my $s = supply { whenever $src.Supply -> $n { emit $n * 2 } };
    my @got;
    $s.tap: -> $v { @got.push: $v };
    $src.emit(1); $src.emit(2);
    is-deeply @got, [2, 4], 'a healthy tap is unaffected';
}

done-testing;

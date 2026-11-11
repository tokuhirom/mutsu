use Test;

# `Supply.Channel` is `self.tap(-> \v { $c.send(v) }, done => { $c.close },
# quit => { $c.fail($_) })`. A supply block that dies after emitting used to
# fail its channel ahead of the values it had already emitted.

plan 4;

{
    my @got;
    my $s = supply { whenever Supply.interval(0.05) -> $v { emit $v } };
    for $s.Channel.list -> $v {
        @got.push: $v;
        last if $v == 2;
    }
    is-deeply @got, [0, 1, 2], 'a live supply block\'s Channel receives values as they come';
}

{
    my $c = supply { emit 1; emit 2; die "oops" }.Channel;
    my @got;
    my $err;
    try { for $c.list { @got.push: $_ }; CATCH { default { $err = .message } } }
    is-deeply @got, [1, 2], 'values before the quit are received first';
    is $err, 'oops', '... and the quit fails the channel after them';
}

{
    my $c = Supply.from-list(1..3).Channel;
    is-deeply $c.list.List, (1, 2, 3), 'a static supply fills and closes its channel';
}

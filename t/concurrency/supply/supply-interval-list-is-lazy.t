use Test;

# `.list` / `.Seq` of a supply backed directly by a native event channel
# (`Supply.interval`, an `IO::Socket::Async` read stream) is lazy: each element
# is pulled from the channel as it arrives, and the stream's done ends the
# list. It used to drain the channel until it closed, so
# `Supply.interval(1).list` never returned (#17428).

plan 3;

{
    my @got;
    for Supply.interval(0.05).list -> $v {
        @got.push: $v;
        last if $v == 2;
    }
    is-deeply @got, [0, 1, 2], 'Supply.interval(...).list yields ticks as they come';
}

is-deeply Supply.interval(0.05).Seq.head(3).List, (0, 1, 2),
    'Supply.interval(...).Seq.head(3)';

{
    my $t = IO::Socket::Async.listen('127.0.0.1', 0).tap: -> $conn { };
    my $port = await $t.socket-port;
    my $conn = await IO::Socket::Async.connect('127.0.0.1', $port);
    $conn.close;
    is-deeply $conn.Supply.list, (), 'the read Supply of a closed socket lists nothing';
    $t.close;
}

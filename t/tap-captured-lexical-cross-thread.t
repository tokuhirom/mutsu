use Test;

# A closure passed to `.tap` is stored and driven asynchronously on another
# thread (the socket-listener worker). A lexical it captures which the parent
# thread reassigns *after* registering the tap must be a shared cell, so the
# tap body observes the new value rather than a stale clone-time snapshot.
# Regression for roast/S32-io/socket-recv-vs-read.t test 11.

plan 2;

{
    my $hostname = 'localhost';
    my $port = 5031;
    my ($send-rest, $client);

    IO::Socket::Async.listen($hostname, $port).tap(-> $conn {
        $conn.print('first thing');
        await $send-rest;
        $conn.print('another thing');
        $conn.close;
    });

    $send-rest = Promise.new;
    my $p = start {
        $client = IO::Socket::INET.new(:host("$hostname:$port"));
        my $res1 = $client.read(20);
        $client.close;
        $res1
    }
    sleep 1;
    is $p.status, Planned,
        'tap body blocks on the parent-reassigned Promise (shared cell)';
    $send-rest.keep(True);
    is $p.result.decode('ascii'), 'first thinganother t',
        'read accumulates across low-level reads once the tap unblocks';
}

# vim: expandtab shiftwidth=4

use Test;

# Closing a listen Tap must not throw away a connection the OS has already
# accepted. The accept thread polls with a 10ms sleep and used to check its
# close flag *before* each `accept()`, so a `Tap.close` that won that race
# dropped an ESTABLISHED connection sitting in the backlog -- even though the
# client's `connect` had already returned, so as far as the client is concerned
# the connection was made. The server then never saw the bytes, and anything
# awaiting them (`$conn.Supply.list` in roast/S32-io/IO-Socket-Async.t) waited
# forever: that file hung in 6 of 8 runs.
#
# Never hardcode a port: listen on 0 and ask the tap which port it got.

plan 2;

{
    my @seen;
    my $accepted = Promise.new;
    my $server = IO::Socket::Async.listen('127.0.0.1', 0);
    my $tap = $server.tap(-> $conn { @seen.push('conn'); $accepted.keep; $conn.close });
    my $port = await $tap.socket-port;

    my $client = await IO::Socket::Async.connect('127.0.0.1', $port);
    # No pause: close the tap while the connection is still only in the backlog.
    $tap.close;
    await Promise.anyof($accepted, Promise.in(5));
    is @seen.elems, 1, 'a connection in the backlog survives Tap.close';
    $client.close;
}

# The bytes that connection sent are delivered too, not just the accept.
{
    my $got = Promise.new;
    my $server = IO::Socket::Async.listen('127.0.0.1', 0);
    my $tap = $server.tap(-> $conn {
        $got.keep($conn.Supply.list.join(''));
    });
    my $port = await $tap.socket-port;

    my $client = await IO::Socket::Async.connect('127.0.0.1', $port);
    await $client.print("still here\n");
    $client.close;
    $tap.close;
    await Promise.anyof($got, Promise.in(5));
    is ($got.status == Kept ?? $got.result !! ''), "still here\n",
        'its bytes reach the server after Tap.close';
}

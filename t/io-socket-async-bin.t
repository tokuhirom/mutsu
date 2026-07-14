use Test;

plan 3;

# IO::Socket::Async connection .Supply(:bin) must emit Buf[uint8], not Str.
# This is the exact point HTTP::Server::Tiny's handler dies: it feeds the
# emitted value to parse-http-request, which expects a Blob, and a Str
# triggers a type error.
#
# Capture via .tap (not `done` inside the Supply whenever) to isolate the
# bin-vs-Str behaviour from the separate react/done teardown path.
{
    # `whenever` taps the listen Supply for us, so there is no ListenSocket to
    # ask for an OS-assigned port (unlike t/io-socket-recv-limit.t, which taps
    # explicitly and uses port 0). Keep an explicit port, but one no other test
    # shares — this file and io-socket-recv-limit.t both used 19995 and killed
    # each other under `prove -j4`.
    my $port = 19996;
    my $got-type = '';
    my $got-bytes;

    my $server = start {
        react {
            whenever IO::Socket::Async.listen("127.0.0.1", $port) -> $conn {
                $conn.Supply(:bin).tap(-> $data {
                    $got-type = $data.^name;
                    $got-bytes = $data;
                });
            }
            whenever Promise.in(1.5) { done }
        }
    };

    sleep 0.5;

    my $client = IO::Socket::INET.new(host => '127.0.0.1', port => $port);
    $client.print("ABC");
    $client.close;

    await $server;

    ok $got-bytes ~~ Blob, "Supply(:bin) emits a Blob/Buf, not a Str";
    is $got-type, 'Buf[uint8]', "emitted value .^name is Buf[uint8]";
    is $got-bytes.list, (65, 66, 67), "bytes are the raw uint8 values of 'ABC'";
}

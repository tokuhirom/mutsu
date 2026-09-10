use Test;

plan 4;

# Test 1: Basic listen + accept + print + close via real TCP
{
    my $port = 19990;
    my $done = False;
    my $response = '';

    # Start server in a thread via start {}
    my $server-promise = start {
        react {
            whenever IO::Socket::Async.listen("127.0.0.1", $port) -> $conn {
                $conn.print("HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nHello\r\n");
                $conn.close;
                $done = True;
                done;
            }
        }
    };

    # Give the server time to bind
    sleep 0.5;

    # Connect as a client using IO::Socket::INET
    my $client = IO::Socket::INET.new(host => '127.0.0.1', port => $port);
    $response = $client.recv;
    $client.close;

    ok $response.contains("Hello"), "Server sent response containing 'Hello'";
    ok $response.contains("200 OK"), "Server sent HTTP 200 OK status";
}

# Test 2: Multiple connections
{
    my $port = 19991;
    my $count = 0;

    my $server-promise = start {
        react {
            whenever IO::Socket::Async.listen("127.0.0.1", $port) -> $conn {
                $count++;
                $conn.print("response $count\r\n");
                $conn.close;
                if $count >= 2 {
                    done;
                }
            }
        }
    };

    sleep 0.5;

    my $r1 = IO::Socket::INET.new(host => '127.0.0.1', port => $port);
    my $resp1 = $r1.recv;
    $r1.close;

    my $r2 = IO::Socket::INET.new(host => '127.0.0.1', port => $port);
    my $resp2 = $r2.recv;
    $r2.close;

    ok $resp1.contains("response"), "First connection received response";
    ok $resp2.contains("response"), "Second connection received response";
}

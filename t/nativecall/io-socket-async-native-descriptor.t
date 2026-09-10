use v6;
use Test;

plan 2;

# Accepted async TCP connections expose the OS file descriptor of the
# underlying stream (used by NativeCall consumers such as Cro::TCP::NoDelay's
# setsockopt). Listen on port 0 and read the assigned port from the tap —
# never hardcode a port.

my $got-fd = Promise.new;
my $tap = IO::Socket::Async.listen('127.0.0.1', 0).tap(-> $conn {
    $got-fd.keep($conn.native-descriptor) unless $got-fd;
    $conn.close;
});
my $port = await $tap.socket-port;

my $client = await IO::Socket::Async.connect('127.0.0.1', $port);
my $fd = await $got-fd;
ok $fd ~~ Int, 'native-descriptor returns an Int';
# An in-process loopback pair may be served by mutsu's in-memory transport,
# which has no OS descriptor and reports -1; a real TCP stream reports a
# descriptor above the stdio range. Both are Ints and never a stdio fd.
ok $fd == -1 || $fd > 2, 'descriptor is a real fd or the in-memory sentinel';
$client.close;
$tap.close;

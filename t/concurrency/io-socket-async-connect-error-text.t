use Test;

plan 2;

# Ask the kernel for an unused port; closing its listener makes both connects
# fail without relying on a fixed port number.
my $listener = IO::Socket::INET.new(:localhost('127.0.0.1'), :localport(0), :listen);
my $port = $listener.localport;
$listener.close;

try { await IO::Socket::Async.connect('127.0.0.1', $port) };
is $!.message, 'connection refused',
    'async connect reports the libuv reason without a host prefix';

try { IO::Socket::INET.connect('127.0.0.1', $port) };
is $!.message, 'Could not connect to socket: Connection refused',
    'synchronous connect keeps its distinct Rakudo wording';

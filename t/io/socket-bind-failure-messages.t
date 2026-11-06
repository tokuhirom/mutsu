use v6;
use Test;

# A bind failure reads the way rakudo reports it: libuv's reason plus the
# address for `IO::Socket::Async.listen` (a quit on the listen Supply), the C
# library's text after "Could not bind socket:" for `IO::Socket::INET`. The
# messages used to carry Rust's wording ("Failed to bind: Cannot assign
# requested address (os error 99)"), which HTTP::Core's t/16 matches against
# 'not available'. The taken port is the one a live
# listener holds. Identical under raku.

plan 5;

# 192.0.2.0/24 is TEST-NET-1 (RFC 5737): never routed, so binding it fails.
{
    my $p = Promise.new;
    my $s = IO::Socket::Async.listen('192.0.2.1', 0);
    $s.tap(-> $c { }, quit => -> $e { $p.keep($e.message) });
    my $msg = await Promise.anyof($p, Promise.in(5)).then({ $p.status ~~ Kept ?? $p.result !! '' });
    like $msg, /^ 'address not available (address: 192.0.2.1:' \d+ ')' $/,
        'Async listen on an address that is not ours';
}

{
    my $held = IO::Socket::INET.new(:listen, :localhost<127.0.0.1>, :localport(0));
    my $port = $held.localport;
    my $err = '';
    try {
        IO::Socket::INET.new(:listen, :localhost<127.0.0.1>, :localport($port));
        CATCH { default { $err = .message } }
    }
    is $err, 'Could not bind socket: Address already in use', 'INET listen on a taken port';
    $held.close;
}

{
    my $held = IO::Socket::INET.new(:listen, :localhost<127.0.0.1>, :localport(0));
    my $port = $held.localport;
    my $p = Promise.new;
    my $s = IO::Socket::Async.listen('127.0.0.1', $port);
    $s.tap(-> $c { }, quit => -> $e { $p.keep($e.message) });
    my $msg = await Promise.anyof($p, Promise.in(5)).then({ $p.status ~~ Kept ?? $p.result !! '' });
    is $msg, "address already in use (address: 127.0.0.1:$port)", 'Async listen on a taken port';
    $held.close;
}

{
    my $err = '';
    try {
        IO::Socket::INET.new(:listen, :localhost<192.0.2.1>, :localport(0));
        CATCH { default { $err = .message } }
    }
    like $err, /^ 'Could not bind socket: ' .+ $/, 'INET listen on an address that is not ours';
    unlike $err, /'os error'/, 'and carries no Rust errno suffix';
}

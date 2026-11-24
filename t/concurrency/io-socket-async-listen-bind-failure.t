use Test;

# A listen whose bind fails and that has no `quit` handler to report to (a
# `whenever` in a `react`) kills the `react` instead of being dropped silently
# (#19058): a `react` that kept a "ready" promise afterwards had its client
# connect to a port nobody listened on, with no sign of why.

plan 3;

my $first = IO::Socket::Async.listen('127.0.0.1', 0).tap(-> $ { });
my $port = await $first.socket-port;
ok $port > 0, 'first listener bound';

my $died = Promise.new;
start {
    react {
        whenever IO::Socket::Async.listen('127.0.0.1', $port) -> $ { }
    }
    CATCH { default { $died.keep(.message) } }
}
await Promise.anyof($died, Promise.in(10));
like $died.status ~~ Kept ?? $died.result !! '', /'address already in use'/,
    'the react died with the bind error';

my $quit = Promise.new;
IO::Socket::Async.listen('127.0.0.1', $port).tap(-> $ { }, quit => { $quit.keep(.message) });
await Promise.anyof($quit, Promise.in(10));
like $quit.status ~~ Kept ?? $quit.result !! '', /'address already in use'/,
    'a tap with quit still gets it there';

$first.close;

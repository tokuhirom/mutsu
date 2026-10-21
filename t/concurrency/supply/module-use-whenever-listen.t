use v6;
use lib 't/lib';
use Test;
use ModuleUseWheneverListener;

plan 1;

# A routine a module's top-level `use` imported is called from a `whenever`
# body that runs on the listener's event loop (#14140).
my $got = Promise.new;
my $accepted = Supplier.new;
my $listen-tap = IO::Socket::Async.listen('127.0.0.1', 0).tap({ $accepted.emit($_) });
my $port = await $listen-tap.socket-port;
my $tap = ModuleUseWheneverListener::incoming($accepted.Supply).tap(
    { $got.keep($_) },
    quit => { $got.keep("quit: " ~ .message) });
await IO::Socket::Async.connect('127.0.0.1', $port);
await Promise.anyof($got, Promise.in(10));
is ($got.status eq 'Kept' ?? $got.result !! 'timeout'), 'noted',
    'a module-imported routine is callable from a listen whenever body';
$tap.close;
$listen-tap.close;

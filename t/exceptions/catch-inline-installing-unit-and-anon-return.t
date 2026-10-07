use v6.e.PREVIEW;
use Test;
use lib 't/lib';
use CatchInlineUser;

# Found via the MCP::Server::Tool::Ask suite (MCP::Server's tools/call).

plan 4;

my $u = CatchInlineUser.new;

is-deeply $u.guarded(sub { die "boom" }), { text => "boom", error => True },
    'CATCH of a method sees its own unit imports when the throw is in another unit';

is-deeply $u.guarded-anon(sub { die "boom" }), { text => "BOOM", error => True },
    '`return` in a CATCH inside an anonymous sub returns from that sub';

sub go(&h) {
    my &run = sub { try { h(); CATCH { default { return .message.uc } } } };
    run();
}
is go(sub { die "boom" }), 'BOOM', 'plain script: return from CATCH in an anon sub';

sub thrower { die "kaboom" }
is go(&thrower), 'KABOOM', 'same, throwing from a named sub';

use Test;

# A sunk statement call to a code variable holding a bare block hands back
# the container of the block's last statement, so a Failure stored in it is
# not sunk; a sub's result is. The sink check now looks at the callee's shape
# only for a value that would explode: every sunk call of an ordinary routine
# used to pay the callee lookup (MCP::Server::Tool::Web's tokenizer).
plan 6;

my $z;
sub f { fail 'boom' }

my &c = { $z = f() };
lives-ok { c(); 1 }, 'a sunk block call leaves the stored Failure alone';

sub s { $z = f() }
dies-ok { s(); 1 }, 'a sunk sub call sinks its Failure';

my sub sp($x) { $x eq ' ' }
lives-ok { sp(' ') for ^3 }, 'sunk calls of a lexical sub';

my $n = 0;
my &bump = -> { $n++ };
bump() for ^4;
is $n, 4, 'a sunk call of a pointy block in a code variable runs';

my &p = { $z = run('false') };
lives-ok { p(); 1 }, 'a sunk block call leaves a stored unsuccessful Proc alone';

sub q { run('false') }
throws-like { q(); 1 }, X::Proc::Unsuccessful, 'a sunk sub call sinks its unsuccessful Proc';

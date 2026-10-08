use Test;

# message, gist, Str, backtrace and payload of the built-in exceptions are rows
# of the method table (ADR-11276 §9.39); the answers are the cascade's.

plan 19;

# --- X::AdHoc
my $e = do { try die "boom"; $! };
isa-ok $e, X::AdHoc, 'die makes an X::AdHoc';
is $e.message, 'boom', 'X::AdHoc.message';
is $e.payload, 'boom', 'X::AdHoc.payload';
is $e.Str, 'boom', 'X::AdHoc.Str';
ok $e.gist.starts-with('boom'), 'X::AdHoc.gist starts with the message';
is X::AdHoc.new(payload => 'p').message, 'p', 'a payload is the message';
is X::AdHoc.new(payload => 'p').Str, 'p', 'a payload is the Str';
is X::AdHoc.new.Str, 'Unexplained error', 'no payload';
is X::AdHoc.new.gist, 'Unexplained error', 'no payload, gist';

# --- Exception
is Exception.new.gist, 'Unthrown Exception with no message', 'no message, gist';
is Exception.new.Str, 'Something went wrong in (Exception)', 'no message, Str';

# --- typed exceptions build their message
my $t = do { try { my Int $i = 'x' }; $! };
isa-ok $t, X::TypeCheck::Assignment, 'a failed typed assignment';
like $t.message, /'Type check failed in assignment'/, 'X::TypeCheck::Assignment.message';
is $t.Str, $t.message, 'Str is the message';
my $n = do { try { Int.foo }; $! };
isa-ok $n, X::Method::NotFound, 'a missing method';
like $n.message, /"No such method 'foo'"/, 'X::Method::NotFound.message';

# --- CX::Warn
my $w;
{
    CONTROL { when CX::Warn { $w = $_; .resume } }
    warn "careful";
}
is $w.message, 'careful', 'CX::Warn.message';
is $w.Str, 'careful', 'CX::Warn.Str';

# --- a user exception keeps its own methods
class MyErr is Exception { method message { 'mine' } }
is MyErr.new.message, 'mine', 'a user message wins';

# vim: expandtab shiftwidth=4

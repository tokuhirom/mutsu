use Test;

# X::AdHoc carries its text in `payload`, not `message` (it is what
# `die "..."` builds). `.message`, `.gist`, AND `.Str` must all surface that
# payload, and `.throw`ing such an exception must propagate the payload text
# (not the type repr `X::AdHoc()`) as the caught message. Previously `.Str`
# returned "X::AdHoc with no message" and a thrown X::AdHoc surfaced
# "X::AdHoc()".

plan 11;

my $e = X::AdHoc.new(payload => "boom");
is $e.message, 'boom', 'X::AdHoc.message is the payload';
is $e.payload, 'boom', 'X::AdHoc.payload';
is $e.Str,     'boom', 'X::AdHoc.Str is the payload (was "with no message")';
is $e.gist,    'boom', 'X::AdHoc.gist is the payload';

# Throwing surfaces the payload as the caught message / stringification.
{
    my $msg;
    try {
        X::AdHoc.new(payload => "kaboom").throw;
        CATCH { default { $msg = .message } }
    }
    is $msg, 'kaboom', 'thrown X::AdHoc caught .message is the payload';
}
{
    my $s;
    try {
        X::AdHoc.new(payload => "kaboom").throw;
        CATCH { default { $s = .Str } }
    }
    is $s, 'kaboom', 'thrown X::AdHoc caught .Str is the payload';
}

# $! after a try holds the payload message.
try { X::AdHoc.new(payload => "splat").throw };
is $!.message, 'splat', '$!.message after thrown X::AdHoc';

# A typed exception thrown surfaces its formatted message.
{
    my $msg;
    try {
        X::NYI.new(feature => "widgets").throw;
        CATCH { default { $msg = .message } }
    }
    is $msg, 'widgets not yet implemented. Sorry.', 'thrown typed exception message';
}

# A user-defined Exception subclass with a custom .message is honored on throw.
my class MyErr is Exception { method message { "custom boom" } }
{
    my $msg;
    try {
        MyErr.new.throw;
        CATCH { default { $msg = .message } }
    }
    is $msg, 'custom boom', 'user Exception custom .message on throw';
}

# `die` with an X::AdHoc instance also surfaces the payload.
try { die X::AdHoc.new(payload => "died-with") };
is $!.message, 'died-with', '$!.message after die X::AdHoc';

# An empty-payload X::AdHoc stringifies to the placeholder.
is X::AdHoc.new.Str, 'Unexplained error', 'empty X::AdHoc.Str placeholder';

use Test;

# An exception class that COMPUTES its message from other attributes — the
# common `method message { $!message //= "…" }` idiom — must have that method
# run wherever raku would call `$exc.message`. mutsu used to read the `message`
# ATTRIBUTE directly in the throw/render paths, so such an exception reported the
# literal text `(Any)`: the attribute is undefined until the method runs.

plan 13;

class X::Resp is Exception {
    has $.rc;
    has $.message;
    method message { $!message //= "Response error: '$.rc'" }
}

is X::Resp.new(:rc('404')).message, "Response error: '404'",
    'a computed message is returned by .message';
is X::Resp.new(:rc('404')).Str, "Response error: '404'",
    'and by .Str';
is X::Resp.new(:rc('404')).gist, "Response error: '404'",
    'and by .gist';

# The throw path must use the method, not the still-undefined attribute.
my $err;
try { X::Resp.new(:rc('500')).throw; CATCH { default { $err = $_ } } };
is $err.message, "Response error: '500'", 'a caught exception carries the computed message';
is $err.Str, "Response error: '500'", 'and stringifies to it';

throws-like { X::Resp.new(:rc('404 Not Found')).throw }, X::Resp,
    message => "Response error: '404 Not Found'",
    'throws-like matches on the computed message';

# `$!` after a `try` sees the same text (this is the RuntimeError message that
# the top-level error report renders).
try { X::Resp.new(:rc('418')).throw };
is $!.Str.lines[0], "Response error: '418'", '$! renders the computed message';

# A plain stored message keeps working.
class X::Plain is Exception { has $.message }
is X::Plain.new(message => 'plain').message, 'plain', 'a stored message still works';
throws-like { X::Plain.new(message => 'plain').throw }, X::Plain, message => 'plain',
    'and is matched by throws-like';

# An exception with nothing to say names its class instead of rendering the
# undefined `message` attribute as `(Any)`.
class Empty is Exception { has $.message }
is Empty.new.gist, 'Unthrown Empty with no message',
    'an unthrown message-less exception gists by class name';
ok Empty.new.message === Any, 'and its .message is undefined';
# A user `method message` that WRITES the private attribute must not recurse.
class X::Write is Exception { has $.message; method message { $!message = 'W'; 'R' } }
is X::Write.new.message, 'R', 'a message method writing $!message does not recurse';

# A user `method gist` still wins over the exception default.
class X::Gist is Exception { has $.message; method gist { 'custom gist' } }
is X::Gist.new(message => 'm').gist, 'custom gist', 'a user .gist wins';

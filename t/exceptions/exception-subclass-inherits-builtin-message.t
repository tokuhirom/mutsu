use Test;

# #18730: a user subclass of a built-in `X::` class inherits its `message`
# (rakudo: `X::OutOfRange.message`), instead of `Exception.message`'s stub.

plan 7;

class MyErr is X::OutOfRange { }
my $e = MyErr.new(got => 3, range => "0..1");
is $e.message, 'Argument out of range. Is: 3, should be in 0..1', '.message';
is $e.Str, 'Argument out of range. Is: 3, should be in 0..1', '.Str';
is $e.gist, 'Argument out of range. Is: 3, should be in 0..1', '.gist';

class Deeper is MyErr { }
is Deeper.new(got => 1, range => "2..3").message,
    'Argument out of range. Is: 1, should be in 2..3', 'through two levels';

class MyNYI is X::NYI { }
is MyNYI.new(feature => "f").message, 'f not yet implemented. Sorry.', 'X::NYI';

class Own is X::OutOfRange { method message { "own" } }
is Own.new(got => 3, range => "0..1").message, 'own', 'a message of its own still wins';

throws-like { MyErr.new(got => 3, range => "0..1").throw }, MyErr,
    message => 'Argument out of range. Is: 3, should be in 0..1', 'thrown';

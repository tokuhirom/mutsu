use Test;

# Found via LLM::Data::Inference (t/15-messageless-exceptions.rakutest): rakudo
# stubs `Exception.message` (`method message() { ... }`), so an Exception
# subclass that never implemented it answers a Failure wrapping X::StubCode, which
# throws as soon as it is used as text. mutsu answered the empty string (and, for
# an `X::`-named class, the same), so "no message" looked like "a message".

plan 11;

class NoMsg is Exception { }
class X::NoMsgPlain is Exception { }
my class X::NoMsgLexical is Exception { }
class Y::Deep::NoMsg is Exception { }

for NoMsg, X::NoMsgPlain, X::NoMsgLexical, Y::Deep::NoMsg -> $class {
    my $m = $class.new.message;
    isa-ok $m, Failure, "{$class.^name}.new.message is a Failure";
    is $m.exception.^name, 'X::StubCode', '... wrapping X::StubCode';
    $m.so;   # handled, so nothing is reported when it is collected
}

# A class that implements `message` is untouched.
class WithMsg is Exception { method message { 'hi' } }
is WithMsg.new.message, 'hi', 'a class with its own message';

# Built-in exceptions keep their text.
is X::AdHoc.new(:payload<boom>).message, 'boom', 'X::AdHoc with a payload';
is X::NYI.new(:feature<f>).message.contains('f'), True, 'a typed built-in message';

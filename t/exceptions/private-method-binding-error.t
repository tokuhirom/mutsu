use Test;

plan 5;

# A private method call whose arguments fail to bind used to report "No such
# private method": the resolver picks a candidate by matching the arguments
# against each overload, and `None` meant both "absent" and "present but does
# not bind". raku reports the binding failure, which is what mutsu's own public
# dispatch already did.

class C {
    method !typed(Int $n) { $n * 2 }
    method !oneArg(Int $n) { $n }
    method go-type() { self!typed('not-an-int') }
    method go-arity() { self!oneArg(1, 2) }
    method go-ok() { self!typed(21) }
}

throws-like { C.new.go-type }, X::TypeCheck::Binding::Parameter,
    'a failed type check reports the binding error';

throws-like { C.new.go-arity }, Exception, message => /'Too many positionals'/,
    'a failed arity check reports the arity error';

is C.new.go-ok, 42, 'a private method that does bind still runs';

# A genuinely absent private method still reports not-found. Compiled through
# EVAL because raku rejects the call at compile time.
throws-like 'class Z { method go() { self!nope() } }; Z.new.go',
    Exception, message => /'No such private method'/,
    'a genuinely absent private method still reports not-found';

# Zero-argument private methods are unaffected.
class E {
    method !hi() { 'hi' }
    method go() { self!hi }
}
is E.new.go, 'hi', 'a zero-argument private method still resolves';

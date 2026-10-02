use Test;
use lib 't/lib';
use FindMethodLazy;

# A user `method ^find_method` answers every method call on its type (#10804,
# the Object::Trampoline / Object::Delayed shape).

plan 16;

class Catch {
    method ^find_method(Mu \type, Str:D $name) {
        method (|c) { "got $name {c.raku}" }
    }
    method real { "real" }
}

is Catch.zip(1, 2), 'got zip \(1, 2)', 'an undeclared method goes through ^find_method';
is Catch.real, 'got real \()', 'so does a declared one';
is Catch.new, 'got new \()', 'and one inherited from Mu';
is Catch.?nope, 'got nope \()', '.? is looked up the same way';
my $name = 'dyn';
is Catch."$name"(3), 'got dyn \(3)', 'a dynamic method name too';
is Catch.^name, 'Catch', 'a metamethod call bypasses ^find_method';
ok Catch.WHAT =:= Catch, '.WHAT bypasses ^find_method';

# The proto/multi pair Object::Trampoline returns from ^find_method: the
# candidates close over the latest call's $name.
class Disp {
    method ^find_method(Mu \type, Str:D $name) {
        my constant &proto-handler = proto method handler(|) {*}
        multi method handler(Disp:U: |args) { "U $name {args.elems}" }
        multi method handler(Disp:D: |args) { "D $name {args.elems}" }
        &proto-handler
    }
}
is Disp.foo(1, 2), 'U foo 2', 'the proto dispatches a type-object invocant';
is Disp.bar, 'U bar 0', 'each call sees its own name';
is Disp.^find_method('x').name, 'handler', '^find_method answers the proto';

# A lazy proxy (t/lib/FindMethodLazy.rakumod): the :D candidate binds its
# invocant raw and replaces the caller's variable with the real object.
my @made;
my $x = slack { @made.push('x'); 42 };
nok $x.defined, '.defined does not build the object';
is +@made, 0, 'nothing built yet';
is $x.succ, 43, 'the first real call builds it and answers';
is @made, <x>, 'built exactly once';
isa-ok $x, Int, 'the variable now holds the real object';
is $x.pred, 41, 'later calls go straight to it';

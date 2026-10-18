use Test;

# `Exception`'s and `Failure`'s control methods are rows (ADR-11276 §9.163):
# `throw`, `rethrow`, `die`, `fail`, `resume` and `Exception.Failure`, reached by
# the receiver's MRO for a built-in or user exception, refused for a type object.

plan 28;

class MyEx is Exception { method message { "my message" } }
class Plain is Exception { has $.message }

sub outcome(&code) {
    try { code(); "lived" } // ($!.^name ~ ": " ~ $!.message)
}

is outcome({ MyEx.new.throw }), 'MyEx: my message', 'a user exception throws';
is outcome({ MyEx.new.rethrow }), 'MyEx: my message', 'a user exception rethrows';
is outcome({ MyEx.new.die }), 'MyEx: my message', 'a user exception dies';
is outcome({ my $r = MyEx.new.fail; $r.^name }), 'MyEx: my message',
    '.fail of a user exception is a Failure that throws when used';
is outcome({ Plain.new(message => "p").throw }), 'Plain: p', 'the message attribute';
is outcome({ X::AdHoc.new(payload => "adhoc").throw }), 'X::AdHoc: adhoc', 'X::AdHoc.throw';
is outcome({ X::AdHoc.new(payload => "adhoc").rethrow }), 'X::AdHoc: adhoc', 'X::AdHoc.rethrow';
is outcome({ X::AdHoc.new(payload => "adhoc").die }), 'X::AdHoc: adhoc', 'X::AdHoc.die';

for <throw rethrow die fail resume> -> $m {
    throws-like { X::NYI."$m"() }, X::Parameter::InvalidConcreteness,
        "X::NYI.$m refuses the type object";
}
throws-like { MyEx.throw }, X::Parameter::InvalidConcreteness,
    'a user exception type object refuses .throw';
throws-like { Exception.throw }, X::Parameter::InvalidConcreteness,
    'Exception.throw refuses the type object';

{
    my $f = X::AdHoc.new(payload => "q").Failure;
    isa-ok $f, Failure, 'Exception.Failure is a Failure';
    nok $f.handled, '... an unhandled one';
    is $f.exception.payload, 'q', '... carrying the exception';
    ok Exception.^can('Failure'), 'Exception.^can("Failure")';
}

sub failing { fail "boom" }
is outcome({ my $f = failing(); $f.die }), 'X::AdHoc: boom', 'Failure.die throws its exception';
is outcome({ my $f = failing(); $f.defined; $f.fail }), 'X::AdHoc: boom',
    'Failure.fail of a handled Failure throws its exception';
is outcome({ my $f = failing(); $f.fail }), 'X::AdHoc: boom',
    'Failure.fail of an unhandled Failure throws its exception';
is outcome({ my $f = failing(); $f.defined; $f.rethrow }), 'X::AdHoc: boom',
    'Failure.rethrow throws its exception';

{
    my class CX::Whatever does X::Control { method message { "<whatever>" } }
    my @controls;
    my @catches;
    {
        CX::Whatever.new.throw;
        CONTROL { default { @controls.push: $_ } }
        CATCH { default { @catches.push: $_ } }
    }
    is +@controls, 1, 'a class doing X::Control throws a control exception';
    is +@catches, 0, '... which CATCH does not see';
}

try { X::AdHoc.new(payload => "bt").throw };
ok $!.backtrace.list.elems > 0, '.throw of a built-in exception carries a backtrace';

{
    my $e = Exception.new;
    try $e.throw;
    ok $e === $!, '.throw raises the invocant itself';
    ok $e.backtrace.list.elems > 0, '... whose own backtrace is the throw site';
}

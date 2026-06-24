use Test;

# When a method invoked on a *type object* (e.g. a `CALL-ME` with a `T:U:`
# invocant) referenced a captured-outer lexical, the slow-path method
# dispatch merged its execution-context `self` (the type object) back into the
# caller's environment, overwriting the caller's instance `self`. A following
# private-attribute assignment `$!attr = ...` then saw a type-object `self`
# and wrongly died "Cannot look up attributes in a <T> type object".

plan 4;

my @registry;

class Status {
    has Int $.code;
    method CALL-ME(Status:U: Int $n) { @registry[$n] }   # refs captured @registry
}
@registry[201] = Status.new(code => 201);
@registry[200] = Status.new(code => 200);

class Response {
    has $.status;
    method set-status(Int $n) {
        $!status = Status($n);       # CALL-ME on type object, assigned to attr
        self;
    }
}

my $r = Response.new;
$r.set-status(201);
is $r.status.code, 201, 'attr assigned from CALL-ME-on-type-object survives';
$r.set-status(200);
is $r.status.code, 200, 'reassignment also works';

# self stays the instance across the type-object call
class C {
    has $.x;
    method go(Int $n) {
        $!x = Status($n);
        self.defined;            # self must still be the instance
    }
}
is C.new.go(200), True, 'self is still the instance after type-object method call';

# ordinary (non-CALL-ME) type-object method referencing captured outer is fine too
my $base = 100;
class T { method calc(Int $n) { $base + $n } }
class D { has $.v; method run(Int $n) { $!v = T.calc($n); $!v } }
is D.new.run(5), 105, 'ordinary type-object method with captured outer';

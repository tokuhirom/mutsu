use v6;
use Test;

plan 6;

# `Exception.throw` takes an optional Backtrace; Red's mapped driver exceptions
# do `method throw { nextwith $!orig-backtrace }`.
class Mapped is Exception {
    has Backtrace $.orig = Backtrace.new;
    method message { "mapped" }
    method throw { nextwith $!orig }
}
throws-like { Mapped.new.throw }, Mapped, message => 'mapped', 'nextwith the stored backtrace reaches Exception.throw';

class Plain is Exception { method message { "plain" } }
throws-like { Plain.new.throw(Backtrace.new) }, Plain, message => 'plain', 'throw with an explicit Backtrace';
throws-like { Plain.new.throw }, Plain, message => 'plain', 'plain throw is unchanged';

my $bt = Backtrace.new;
my $caught;
try { Plain.new.throw($bt); CATCH { default { $caught = $_ } } }
ok $caught ~~ Plain, 'the exception is caught';
ok $caught.backtrace.defined, 'it carries a backtrace';
lives-ok { try Plain.new.rethrow }, 'rethrow of an unthrown exception is still an exception, not a crash';

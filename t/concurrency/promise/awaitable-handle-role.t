use Test;

plan 16;

# The core Awaitable::Handle role (ADR-15519): four attributes, two
# already-settled factories and a required subscribe-awaiter. The output is
# identical under rakudo.
class H does Awaitable::Handle { method subscribe-awaiter(&s) { s(True, 1) } }

is Awaitable::Handle.^name, 'Awaitable::Handle', 'the role has its name';
is Awaitable::Handle.^attributes.map(*.name).join(' '), '$!already $!success $!result $!cause',
    'the role carries rakudo\'s four attributes';

my $ok = H.already-success(42);
ok $ok.already, 'already-success: already';
ok $ok.success, 'already-success: success';
is $ok.result, 42, 'already-success: result';
is $ok.cause.^name, 'Exception', 'already-success: cause is the Exception type object';
isa-ok $ok, H, 'the factory builds an instance of the invocant class';

my $bad = H.already-failure(X::AdHoc.new(payload => 'bad'));
ok $bad.already, 'already-failure: already';
nok $bad.success, 'already-failure: not success';
is $bad.cause.message, 'bad', 'already-failure: cause';
is $bad.result.^name, 'Mu', 'already-failure: result is untouched';

ok $ok ~~ Awaitable::Handle, 'an instance does the role';
is H.new.already.raku, 'Bool', 'a plain instance is unsettled';

sub want-handle(Awaitable::Handle:D $) { 'accepted' }
is want-handle($ok), 'accepted', 'a handle satisfies Awaitable::Handle:D';
nok (try want-handle(H)).defined, 'a type object does not satisfy :D';

try EVAL 'class NoSubscribe does Awaitable::Handle {}';
like $!.message, /"Method 'subscribe-awaiter' must be implemented by NoSubscribe"/,
    'omitting subscribe-awaiter is a compile error';

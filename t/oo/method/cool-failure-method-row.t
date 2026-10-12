use Test;

plan 7;

my $f = "too short".Failure;
isa-ok $f, Failure, "Str.Failure is a Failure";
isa-ok $f.exception, X::AdHoc, "wrapping an X::AdHoc";
is $f.exception.payload, "too short", "payload is the invocant";
is $f.exception.message, "too short", "message is its stringification";
ok !$f.handled, "not handled yet";
$f.so;

is 42.Failure.exception.message, "42", "an Int is a Cool too";

fails-like { "Target length 2 is too short".Failure }, X::AdHoc,
  :payload("Target length 2 is too short"), "usable with fails-like";

done-testing;

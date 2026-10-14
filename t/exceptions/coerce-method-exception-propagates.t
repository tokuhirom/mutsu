use Test;

plan 8;

# An exception thrown inside a user `COERCE` method propagates as itself: it is
# neither X::Coerce::Impossible nor a binding failure of the callee, and the
# enclosing call is not re-run.
my $calls = 0;
class C { method COERCE($v) { $calls++; die "boom" } }
sub k(C() $x) { $x }

try { k(42) };
is $!.^name, "X::AdHoc", "class target: the thrown exception propagates";
is $!.message, "boom", "...with its own message";
is $calls, 1, "COERCE ran once";

$calls = 0;
throws-like { k(42) }, Exception, message => 'boom';
is $calls, 1, "COERCE ran once under throws-like (the subtest is not re-run)";

role R { method COERCE($v) { die "rboom" } }
sub r(R() $x) { $x }
try { r(1) };
is $!.^name, "X::AdHoc", "role target: the thrown exception propagates";
is $!.message, "rboom", "...with its own message";

class D { multi method COERCE(Str $s) { D.new } }
sub d(D() $x) { $x }
throws-like { d(1) }, X::Coerce::Impossible, "no candidate for the value is still an impossible coercion";

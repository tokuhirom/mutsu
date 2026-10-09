use Test;

# A `sub` declared inside a Lock.protect block must stay callable after the
# block returns (found via the hide-methods distribution, which wraps methods
# with a sub created inside $lock.protect).
plan 4;

my $lock = Lock.new;
my $kept;
$lock.protect: { sub inner($x) { "inner $x" }; $kept = &inner; 1 };
is $kept(1), "inner 1", "nested sub callable after protect";

class C { method bar { "bar" } }
my $m := C.^find_method('bar');
$lock.protect: { sub w(\SELF, |c) { "wrapped" }; $m.wrap(&w); 1 };
is C.bar, "wrapped", "a wrapper declared in protect is called";

my $inside;
$lock.protect: { sub f() { 42 }; $inside = f() };
is $inside, 42, "nested sub callable inside protect";
is $lock.protect({ sub g { 7 }; g() }), 7, "nested sub result returned";

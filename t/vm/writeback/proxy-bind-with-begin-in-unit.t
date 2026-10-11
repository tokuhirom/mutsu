use Test;

# `my $x := Proxy.new(...)` binds the Proxy even when a BEGIN later in the
# unit makes the prologue split the unit's declarations: the declaration must
# not turn into an assignment, which would FETCH the Proxy into a plain Scalar.

plan 2;

my $s = "abc";
my $x := Proxy.new(FETCH => -> | { $s }, STORE => -> | { 1 });
my $m := Proxy.new(FETCH => method { $s }, STORE => method (\v) { 1 });
BEGIN { 1 }
$s = "zzz";
is $x, "zzz", 'a bound Proxy keeps FETCHing (lambda)';
is $m, "zzz", 'a bound Proxy keeps FETCHing (method)';

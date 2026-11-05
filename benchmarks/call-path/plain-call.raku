# Call-path harness input (ADR-15737 slice 1): the plain Raku-level method call.
# One iteration = construct nothing, call a one-argument method and a
# zero-argument accessor-style method on a fixed object. scripts/call-path-ab.sh
# runs it at two iteration counts and reports the per-iteration slope, so the
# startup and the first-call warm-up cancel out.
#
#   CALLPATH_N=<iterations>  (default 200)
class Counter {
    has Int $.n = 0;
    method bump(Int $by) { $!n + $by }
    method value() { $!n }
}

my $n = (%*ENV<CALLPATH_N> // 200).Int;
my $c = Counter.new(n => 3);
my $sum = 0;
for ^$n {
    $sum += $c.bump(1);
    $sum += $c.value;
}
say $sum;

use Test;
use NativeCall;

# A `sub` declared in a role body has its traits applied once, at the role's
# declaration, however often the role is composed, parameterized or punned;
# each composition still gets a working routine (#18097).

plan 7;

my $code = q:to/CODE/;
multi trait_mod:<is>(Routine $r, :$probe!) { note "probe {$r.name}" }
role R { sub g($a) is probe { $a * 2 }; method z($v) { g($v) } }
class C does R { }
class D does R { }
my role P[$x] { sub h is probe { 'h' }; method hx { h() ~ $x } }
class E does P[1] { }
class F does P[2] { }
say R.new.z(4), ' ', C.new.z(1) + D.new.z(2), ' ', E.new.hx, ' ', F.new.hx;
CODE

my $proc = run $*EXECUTABLE, '-e', $code, :out, :err;
my $out = $proc.out.slurp(:close);
my @probes = $proc.err.slurp(:close).lines.sort;
is $out, "8 6 h1 h2\n", 'every composition, parameterization and pun calls its sub';
is @probes.grep('probe g').elems, 1, 'the trait on the plain role\'s sub ran once';
is @probes.grep('probe h').elems, 1, 'the trait on the parametric role\'s sub ran once';
is @probes.elems, 2, 'and nothing else probed';

# A trait that rebinds the routine's body (NativeCall's `is native`) keeps
# working in every composition.
role N { sub strlen(Str --> size_t) is native { * }; method len($s) { strlen($s) } }
class A does N { }
class B does N { }
is A.new.len('abc'), 3, 'a native sub in the first composition';
is B.new.len('abcd'), 4, 'and in the second';
is N.new.len('ab'), 2, 'and in the pun';

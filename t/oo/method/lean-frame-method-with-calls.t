use Test;

# A planned method whose body calls something now runs in the lean frame,
# which on exit carries the frame's pending runtime-name writes out the way
# the fast binder did (#18267). Every write a callee makes must still reach
# the caller: through a dynamic variable, an outer lexical, a run-time name,
# an EVAL, and a nested method chain. Each case runs several times so later
# calls replay the plan.
plan 10;

class Wrap {
    has $.n = 0;
    method chars-of(Str $s) { $s.chars + $!n }
    method scaled($r) { ($r * 4).Int }
    method chain($x) { self.chars-of($x) + self.scaled($x.chars) }
}
my $w = Wrap.new(n => 1);
is (^3).map({ $w.chars-of('ab' x $_) }).join(','), '1,3,5', 'a body that calls a builtin method';
is (^3).map({ $w.scaled($_ / 2) }).join(','), '0,2,4', 'a body that coerces through a call';
is (^3).map({ $w.chain('x' x $_) }).join(','), '1,6,11', 'a body that calls other methods';

my $*dyn = 0;
sub bump-dyn() { $*dyn++ }
class Dyn { method go($x) { bump-dyn(); $x } }
my $d = Dyn.new;
$d.go($_) for ^4;
is $*dyn, 4, 'a callee writes a dynamic variable of the caller';

my $outer = 0;
sub bump-outer() { $outer += 10 }
class Out { method go($x) { bump-outer(); $x + 1 } }
my $o = Out.new;
my @r = (^3).map({ $o.go($_) });
is "@r[] $outer", '1 2 3 30', 'a callee writes an outer lexical';

our $pkg = 0;
class Pkg { method go($x) { $GLOBAL::pkg = $x; $x } }
Pkg.new.go($_) for ^3;
is $pkg, 2, 'a package variable written by name';

my $named = 'start';
class Named { method go($v) { EVAL '$named = $v'; $v } }
my $n = Named.new;
$n.go("v$_") for ^3;
is $named, 'v2', 'an EVAL in the method writes a caller lexical';

my $rt = 0;
sub set-rt($v) { ::('$rt') = $v }
class Rt { method go($v) { set-rt($v); $v } }
Rt.new.go($_) for 1..3;
is $rt, 3, 'a run-time named write in a callee';

my $*seen = '';
class Topic { method go($x) { my $*seen = 'inner'; inner-read() ~ $x } }
sub inner-read() { $*seen }
is (^2).map({ Topic.new.go($_) }).join(','), 'inner0,inner1', 'a dynamic declared in the method is seen by its callee';
is $*seen, '', '...and does not leak to the caller';

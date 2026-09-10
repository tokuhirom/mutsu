use Test;

# Pin for docs/vm-decoupling.md: a 0-arg public attribute-accessor read on an
# Instance (`$obj.x`) is handled by the VM's shared `try_fast_accessor_read`
# fast path on BOTH the non-mut (CallMethod) and mut (CallMethodMut) opcodes.
# A method call on a *variable* compiles to CallMethodMut, so without the mut
# path fast-path every accessor read on a lexical fell back to the interpreter.
# The fast path must preserve all accessor semantics.

plan 12;

class Point { has $.x; has $.y; }

# Read via a lexical variable (CallMethodMut path) and via a temporary
# (CallMethod path) must both work.
my $p = Point.new(x => 3, y => 4);
is $p.x, 3, 'accessor read via variable (mut opcode)';
is $p.y, 4, 'second accessor read via variable';
is Point.new(x => 7, y => 8).x, 7, 'accessor read via temporary (non-mut opcode)';

# rw accessor: read, then write, then read again.
class Cfg { has $.v is rw; }
my $c = Cfg.new(v => 1);
is $c.v, 1, 'rw accessor initial read';
$c.v = 99;
is $c.v, 99, 'rw accessor read after write';

# Private attribute must NOT be reachable as a public accessor from outside.
class Sec { has $!secret = 42; method peek() { $!secret } }
my $s = Sec.new;
is $s.peek, 42, 'private attr readable via own method';
nok (try { $s.secret }).defined, 'private attr NOT readable as a public accessor';

# Inherited accessor.
class Base { has $.name; }
class Derived is Base { }
is Derived.new(name => "hi").name, "hi", 'inherited accessor read';

# A user-defined method whose name matches an attribute must take priority over
# the auto-accessor (the fast path must defer to has_user_method).
class Over { has $.raw = 10; method raw() { 999 } }
is Over.new.raw, 999, 'user method shadows accessor-name read';

# An unset public accessor returns the type/Nil rather than throwing.
class Maybe { has $.opt; }
nok Maybe.new.opt.defined, 'unset accessor returns undefined';

# Accessor read inside a hot loop stays correct (regression-style check).
my $q = Point.new(x => 5, y => 6);
my $sum = 0;
$sum += $q.x + $q.y for ^100;
is $sum, 1100, 'accessor reads in a loop accumulate correctly';

# Method call with args on the same object still dispatches normally.
class Calc { has $.base; method add($n) { $.base + $n } }
is Calc.new(base => 10).add(5), 15, 'non-accessor method with args still works';

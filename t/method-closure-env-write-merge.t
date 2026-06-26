use Test;

# A method whose body INVOKES A CLOSURE that writes an outer-scope variable
# (dynamic var, captured-outer lexical, or global) must merge that write back to
# the caller. The compiled method fast path's can_skip_merge gated only on
# has_env_writes, which omits the closure-invocation ops (CallOnValue/
# CallOnCodeVar) and CallDefined/ExecCallSlip — so the write was silently dropped
# (a method `{ my $f = { $*x = 1 }; $f() }` left $*x unchanged). The closure
# dispatch already gated on has_calls || has_env_writes; the method path did not.

plan 7;

# Dynamic variable written by a nested closure inside a method.
my $*DV = 'orig';
class A { method go { my $f = { $*DV = 'changed' }; $f() } }
A.new.go;
is $*DV, 'changed', 'dynamic var written by a nested closure in a method propagates';

# Captured-outer lexical written by a nested closure inside a method.
my $lex = 0;
class B { method bump { my $f = { $lex = 42 }; $f() } }
B.new.bump;
is $lex, 42, 'captured-outer lexical written by a nested closure in a method propagates';

# Two dynamic vars written by a nested closure (the grammar delimiter shape).
my $*L = '{';
my $*R = '}';
class C { method delim { my $f = { $*L = '<'; $*R = '>'; }; $f() } }
C.new.delim;
is "$*L$*R", '<>', 'both dynamic vars written by a nested closure propagate';

# A direct dynamic-var write from a method still works (was never broken).
my $*D2 = 'a';
class D { method set { $*D2 = 'b' } }
D.new.set;
is $*D2, 'b', 'direct dynamic-var write from a method still propagates';

# A method calling a named sub that writes a dynamic var via .() also merges.
my $*VIA = 'x';
sub writer { $*VIA = 'y' }
class E { method run { my $w = &writer; $w() } }
E.new.run;
is $*VIA, 'y', 'dynamic var written through a code-var call in a method propagates';

# Nested closure increment of a captured-outer counter accumulates across calls.
my $count = 0;
class F { method tick { my $f = { $count++ }; $f() } }
my $f = F.new;
$f.tick; $f.tick; $f.tick;
is $count, 3, 'nested-closure captured-outer increment accumulates across method calls';

# A pure method (no calls, no env writes) still skips the merge correctly.
class G { has $.v; method read { $.v } }
is G.new(v => 9).read, 9, 'a pure read-only method still returns correctly (merge-skip intact)';

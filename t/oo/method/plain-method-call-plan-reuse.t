use v6;
use Test;

# A warm plain method call reuses its resolution and binding decision
# (ADR-15737 D1: the method-call plan behind the plain-method lane, and the
# per-method frame plan that seeds its locals). Every call below runs in a
# loop, so the later iterations take the reused plan; each must still answer
# what the full dispatch answers, including after the registry changes under
# a warm plan (a new method, a wrap, an augment) and for receivers whose
# shape the plan must not cover.

plan 20;

class Counter {
    has $.n = 0;
    has @.log;
    has %.seen;
    method bump($by) { $!n += $by; @!log.push($by); %!seen{$by}++; self }
    method get() { $!n }
    method sum(Int $a, Int $b) { $a + $b }
    method maybe($x?) { $x // 'none' }
    method fail-on-odd(Int $x) { die "odd $x" if $x % 2; $x }
    method early($x) { return 'neg' if $x < 0; 'pos' }
}

my $c = Counter.new;
$c.bump($_) for 1..5;
is $c.get, 15, 'a method that writes attributes on every call';
is $c.log.join(','), '1,2,3,4,5', 'an array attribute written on every call';
is %($c.seen).keys.sort.join(','), '1,2,3,4,5', 'a hash attribute written on every call';

is (^4).map({ $c.sum($_, 10) }).join(','), '10,11,12,13', 'two typed positionals';
is (^3).map({ $c.maybe($_) }).join(','), '0,1,2', 'an optional positional, supplied';
is (^3).map({ $c.maybe }).join(','), 'none,none,none', 'an optional positional, omitted';

my @got;
for 0..3 -> $i {
    @got.push: (try $c.fail-on-odd($i)) // "died:{$!.message}";
}
is @got.join(','), '0,died:odd 1,2,died:odd 3', 'an exception from a warm call';

is (-2..1).map({ $c.early($_) }).join(','), 'neg,neg,pos,pos', 'an explicit return';

dies-ok { $c.sum('a', 1) for ^3 }, 'a call with other argument types still type-checks';

# Two receivers of different classes share nothing.
class Sub is Counter { method get() { 'sub:' ~ callsame } }
my $s = Sub.new(n => 7);
is (^3).map({ $s.get }).join(','), 'sub:7,sub:7,sub:7', 'a subclass override with callsame';
is (^3).map({ $c.get }).join(','), '15,15,15', 'the parent class keeps its own method';

# The registry changes under a warm plan.
class Late { method v() { 'old' } }
my $l = Late.new;
my @v = (^3).map({ $l.v });
Late.^add_method('w', method () { 'added' });
@v.push: $l.w for ^2;
is @v.join(','), 'old,old,old,added,added', 'a method added after the calls warmed up';

class Wrapped { method v($x) { "v$x" } }
my $w = Wrapped.new;
my @wv = (^3).map({ $w.v($_) });
Wrapped.^find_method('v').wrap(-> $self, $x { 'w' ~ callsame });
@wv.push: $w.v($_) for 3..4;
is @wv.join(','), 'v0,v1,v2,wv3,wv4', 'a wrap installed after the calls warmed up';

use MONKEY-TYPING;
class Aug { method v() { 'before' } }
my $a = Aug.new;
my @av = (^3).map({ $a.v });
augment class Aug { method u() { 'augmented' } }
@av.push: $a.u for ^2;
@av.push: $a.v;
is @av.join(','), 'before,before,before,augmented,augmented,before', 'an augment after warm-up';

# Role-composed methods see their role context.
role R { method who() { $?ROLE.^name ~ '/' ~ self.^name } }
class WithR does R { }
my $r = WithR.new;
is (^3).map({ $r.who }).join(','), 'R/WithR,R/WithR,R/WithR', '$?ROLE in a composed method';

# A sigilless attribute (alias metadata) keeps its own binding.
class Alias { has $x; method set($v) { $x = $v; self }; method get() { $x } }
my $al = Alias.new;
$al.set($_) for ^4;
is $al.get, 3, 'a sigilless attribute written on every call';

# A method writing an outer lexical (no env-merge skip).
my $outer = 0;
class Outer { method touch($v) { $outer += $v } }
my $o = Outer.new;
$o.touch($_) for 1..4;
is $outer, 10, 'a method writing an outer lexical on every call';

# Recursion through the same plan.
class Fib { method f(Int $n) { $n < 2 ?? $n !! self.f($n - 1) + self.f($n - 2) } }
is Fib.new.f(15), 610, 'a recursive method';

# A method returning a Proxy is fetched unless rw.
class P {
    has $!store = 41;
    method px() { Proxy.new(FETCH => -> $ { $!store + 1 }, STORE => -> $, $v { $!store = $v }) }
}
my $p = P.new;
is (^3).map({ $p.px }).join(','), '42,42,42', 'a returned Proxy is fetched';

# A sigilless parameter binds by where the argument came from: a literal is a
# read-only term, a variable is aliased. The two calls share a type key.
class Sig { method set(\i, $v) { i = $v } }
my $sg = Sig.new;
my @ro;
for ^2 { @ro.push: (try { $sg.set(3, 4); 'ok' }) // 'died' }
my $sv = 1;
$sg.set($sv, $_) for 5..6;
is "{@ro.join(',')} $sv", 'died,died 6', 'a sigilless parameter after a literal call warmed up';

use v6;
use Test;

# VM-native default construction now covers classes whose only non-simple
# feature is a `TWEAK` submethod (Track A ③ constructor). The instance is
# assembled natively (named args + attribute defaults), then the TWEAK phase
# runs via the shared `run_tweak_phase` helper — the same ordering/dispatch the
# full constructor uses. These pin the semantics that must survive the move.

plan 16;

# --- basic derived attribute in TWEAK ---
class Temp {
    has $.celsius;
    has $.fahrenheit;
    submethod TWEAK { $!fahrenheit //= $!celsius * 9/5 + 32 }
}
is Temp.new(celsius => 100).fahrenheit, 212, 'TWEAK computes a derived attribute';
is Temp.new(celsius => 0, fahrenheit => 50).fahrenheit, 50,
    'a provided value is kept when TWEAK only defaults it';

# --- TWEAK mutates a typed scalar attribute with a default ---
class B { has Int $.x = 5; submethod TWEAK { $!x = $!x * 2 } }
is B.new.x, 10, 'TWEAK mutates a defaulted typed attribute';
is B.new(x => 7).x, 14, 'TWEAK mutates a provided typed attribute';

# --- coercion-typed attribute is coerced before TWEAK observes it ---
class A { has Int() $.n; method kind { $!n.WHAT.^name } }
class A2 { has Int() $.n; has $.seen; submethod TWEAK { $!seen = $!n.WHAT.^name } }
is A.new(n => "42").n, 42, 'coercion-typed attribute coerces in the native path';
is A2.new(n => "42").seen, 'Int', 'TWEAK sees the coerced value, not the raw Str';

# --- TWEAK that dies propagates the exception ---
class C { has $.age; submethod TWEAK { die "negative age" if $!age < 0 } }
dies-ok { C.new(age => -1) }, 'a dying TWEAK propagates out of .new';
is C.new(age => 3).age, 3, 'a non-dying TWEAK constructs normally';

# --- TWEAK derives one attribute from another provided one ---
class F { has $.a; has $.b; submethod TWEAK { $!b = $!a + 1 } }
is F.new(a => 10).b, 11, 'TWEAK derives an attribute from a provided one';

# --- inheritance: base + child TWEAK both run, base-first ---
class Base { has @.order is rw; }
class Mid is Base { submethod TWEAK { self.order.push("Mid") } }
class Leaf is Mid { submethod TWEAK { self.order.push("Leaf") } }
is Leaf.new.order.join(","), "Mid,Leaf",
    'TWEAK runs base-first across the inheritance chain';

# --- typed container attribute is finalized before TWEAK ---
class E { has Int @.nums; has $.total; submethod TWEAK { $!total = @!nums.sum } }
is E.new(nums => [1, 2, 3]).total, 6, 'TWEAK sees a finalized typed container';

# --- an empty TWEAK is a no-op ---
class N { has $.v = 9; submethod TWEAK { } }
is N.new.v, 9, 'an empty TWEAK leaves attributes untouched';

# --- TWEAK coexisting with BUILD still runs through the full constructor ---
class G {
    has $.a;
    has $.b;
    submethod BUILD(:$a) { $!a = $a // 1 }
    submethod TWEAK { $!b = $!a * 10 }
}
is G.new(a => 5).b, 50, 'BUILD+TWEAK class constructs correctly (full path)';
is G.new.b, 10, 'BUILD+TWEAK class uses BUILD defaults then TWEAK';

# --- TWEAK with a named-arg signature receives the constructor args ---
class H { has $.y; submethod TWEAK(:$y) { $!y = 2 * $y } }
is H.new(y => 3).y, 6, 'TWEAK(:$y) binds the named constructor argument';

# --- parent TWEAK and child TWEAK(:$y) both see the args, base-first ---
class PT { has $.x; submethod TWEAK { $!x *= 2 } }
class CT is PT { has $.y; submethod TWEAK(:$y) { $!y = 2 * $y } }
my $ct = CT.new(x => 1, y => 2);
is "{$ct.x},{$ct.y}", "2,4", 'parent and child TWEAK(:$y) both bind args base-first';

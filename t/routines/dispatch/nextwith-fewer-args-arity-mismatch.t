use Test;

# #8654: nextwith/callwith called with FEWER args than the original call could
# silently fail to reach a parent-class method whose own arity matches the
# nextwith/callwith args, not the original call's. The deferral candidate list
# was built once, up front, filtered by the ORIGINAL call's argument shape —
# so a non-multi ancestor override whose signature only fit the (smaller)
# nextwith/callwith argument list was wrongly excluded before nextwith/callwith
# ever ran.

plan 4;

class Base {
    has $.body = "";
    method body-set($text) { $!body = $text; }
}
class Derived is Base {
    method body-set($body, $super?) {
        nextwith($body);
    }
}

my $d1 = Derived.new;
$d1.body-set("hello");
is $d1.body, "hello", "1-arg call reaches the parent via nextwith with fewer args";

my $d2 = Derived.new;
$d2.body-set("hello2", True);
is $d2.body, "hello2", "2-arg call (extra arg not forwarded) still reaches the parent";

class Base2 {
    has $.val = "";
    method set($x) { $!val = $x; }
}
class Derived2 is Base2 {
    method set($x, $extra?) {
        callwith($x);
    }
}

my $c = Derived2.new;
$c.set("a", "b");
is $c.val, "a", "callwith with fewer args also reaches an arity-matching ancestor";

class GP3 { method m($a, $b) { "gp($a,$b)" } }
class P3 is GP3 { method m($a) { nextwith($a, "default") } }
class C3 is P3 { method m() { nextwith("only-c") } }
is C3.new.m, "gp(only-c,default)",
    "a chain of nextwith calls with progressively different arities still resolves each hop";

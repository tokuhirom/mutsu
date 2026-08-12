use Test;

# ADR-0019 E9-pre ground-truth pin (verified against Rakudo v2026.06, 2026-08-12):
# callwith/nextwith re-bind args but keep `is rw` containers live (a parent's
# assignment writes back to the caller's variable), and callwith continues with
# the NEXT candidate rather than restarting.

plan 5;

class P { method m($x is rw) { $x = 99; "p" } }
class C is P { method m($x is rw) { my $r = callwith($x); "c-$r" } }
my $v = 1;
is C.new.m($v), "c-p", "callwith passes the parent's return back";
is $v, 99, "rw container survives callwith re-binding";

class C2 is P { method m($x is rw) { nextwith($x) } }
my $w = 5;
is C2.new.m($w), "p", "nextwith passes the return through";
is $w, 99, "rw container survives nextwith re-binding";

class Q {
    multi method m(Int $x) { my $r = callwith($x + 1); "q-$r" }
    multi method m(Any $x) { "any-$x" }
}
is Q.new.m(10), "q-any-11", "callwith advances to the NEXT candidate with new args";

use Test;

# ADR-0019 Phase F box F1 (todo/tickets/classhow-lookup-returns-sub-not-
# method-instance.md): `.^lookup`/`.^find_method` used to return a
# `Sub`/`Routine`-shaped value instead of the `Method`/`Submethod` `Instance`
# `.^methods`/`.^method_table` build, so calling the result directly
# (`$m(invocant, args)`), which real Raku supports via `Method`'s implicit
# `CALL-ME`, raised "No such method 'CALL-ME' for invocant of type 'Method'".
# This pin fixes the representation and verifies calling still runs the exact
# method that was looked up -- byte-for-byte checked against Rakudo v2026.06.

class Plain {
    method foo(Int $x) { $x + 1 }
}
my $p = Plain.new;
my $m1 = Plain.^lookup("foo");
is $m1.WHAT.gist, '(Method)', 'lookup of a plain method is a Method Instance';
is $m1($p, 5), 6, 'calling a plain method lookup runs the method';
is $m1.candidates.elems, 1, 'a non-multi method is its own sole candidate';

class Multi1 {
    multi method bar(Int $x) { "int:$x" }
    multi method bar(Str $x) { "str:$x" }
}
my $mm = Multi1.new;
my $m2 = Multi1.^lookup("bar");
is $m2($mm, 5), 'int:5', 'calling a multi dispatcher lookup re-dispatches on Int arg';
is $m2($mm, "hi"), 'str:hi', 'calling a multi dispatcher lookup re-dispatches on Str arg';
is $m2.candidates[0]($mm, 5), 'int:5', 'calling an individual multi candidate runs that candidate';

role R {
    method baz(Int $x) { $x * 2 }
}
class WithRole does R {}
my $m3 = WithRole.^lookup("baz");
is $m3(WithRole.new, 5), 10, 'calling a role method lookup runs the role method';

# Native methods: F1's mechanism slice already synthesizes .signature/.package
# defaults for these; this pin adds direct callability on top.
my $m4 = (4.5).^lookup("floor");
is $m4(4.5), 4, 'calling a native method lookup runs the native method';

# .^find_method shares classhow_lookup_impl and must behave the same way.
my $m5 = Plain.^find_method("foo");
is $m5($p, 9), 10, 'calling a .^find_method result runs the method';

done-testing;

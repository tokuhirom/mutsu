use v6;
use Test;

plan 12;

my role R[::T] { multi method foo(T $t) { "T=" ~ T.^name } }

# Same parametric role composed twice with different type args:
# each candidate's body must see ITS OWN T binding.
my class A does R[Int] does R[Str] { }
is A.new.foo(5),   "T=Int", "Int arg selects the R[Int] candidate and binds T=Int";
is A.new.foo("x"), "T=Str", "Str arg selects the R[Str] candidate and binds T=Str";

# Swapped composition order must not change the outcome.
my class B does R[Str] does R[Int] { }
is B.new.foo(5),   "T=Int", "swapped does order: Int arg still binds T=Int";
is B.new.foo("x"), "T=Str", "swapped does order: Str arg still binds T=Str";

# Call order must not matter either (no first-call cache poisoning).
my class C does R[Int] does R[Str] { }
is C.new.foo("x"), "T=Str", "Str-typed first call binds T=Str";
is C.new.foo(5),   "T=Int", "Int-typed second call binds T=Int";

# An argument matching neither candidate dies (both candidates must
# survive into dispatch with correctly substituted signatures).
dies-ok { A.new.foo(3.5) }, "Rat arg matches neither Int nor Str candidate";

# A class-body multi of the same name coexists with the role candidates.
my class D does R[Int] does R[Str] { multi method foo(Rat $t) { "class-Rat" } }
is D.new.foo(3.5), "class-Rat", "class-body Rat candidate wins for Rat";
is D.new.foo(5),   "T=Int",     "role Int candidate still selected alongside class multi";
is D.new.foo("x"), "T=Str",     "role Str candidate still selected alongside class multi";

# Single composition (the already-working shape) keeps working.
my class E does R[Int] { }
is E.new.foo(5), "T=Int", "single composition binds T=Int";
dies-ok { E.new.foo("x") }, "single composition rejects a Str arg";

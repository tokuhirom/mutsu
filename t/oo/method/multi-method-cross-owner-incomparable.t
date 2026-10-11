use Test;

# Multi-method candidates of DIFFERENT classes in one MRO are ranked by the
# same `is_narrower` partial order as one class's candidates (#12468). When an
# ancestor's candidate and a subclass's are incomparable -- each is narrower
# on a different parameter, and one of them carries a refinement (`where`) --
# neither out-ranks the other, so the one declared first wins. Rakudo derives
# a subclass's multi dispatcher from its parent's, so the ancestor's
# candidate is the one declared first. Every expectation below is rakudo's
# answer; mutsu used to rank such a pair by summed type distance or by the
# refinement count and pick the subclass's candidate.

plan 6;

# The subclass's candidate refines its first parameter; the ancestor's is
# nominally narrower there, the subclass's on the second.
class P1 { multi method m(Int $x, Any $y) { "P1" } }
class C1 is P1 { multi method m(Any $x where True, Int $y) { "C1" } }
is C1.new.m(1, 2), "P1", "incomparable with a refined subclass candidate: the ancestor's wins";

# The same with the refinement on the ancestor's side.
class P2 { multi method m(Int $x, $y where True) { "P2" } }
class C2 is P2 { multi method m(Cool $x, Int $y) { "C2" } }
is C2.new.m(1, 2), "P2", "incomparable with a refined ancestor candidate: the ancestor's wins";

# A smaller summed distance does not make the subclass's candidate narrower.
class P3 { multi method m(Any $x where True, Int $y) { "P3" } }
class C3 is P3 { multi method m(Int $x, Numeric $y) { "C3" } }
is C3.new.m(1, 2), "P3", "the subclass's smaller type distance does not decide an incomparable pair";

# Three levels: the middle candidate out-narrows the subclass's on both
# parameters and drops it; it is incomparable with the root's, which wins.
class G4 { multi method m(Int $x, Numeric $y) { "G4" } }
class P4 is G4 { multi method m(Numeric $x, Int $y where True) { "P4" } }
class C4 is P4 { multi method m(Any $x where True, Int $y) { "C4" } }
is C4.new.m(1, 2), "G4", "a dominated candidate drops out, the first declared incomparable one wins";

# A comparable pair is still decided by narrowness, whoever owns it.
class P5 { multi method m(Int $x where True, Int $y) { "P5" } }
class C5 is P5 { multi method m(Int $x, Int $y) { "C5" } }
is C5.new.m(1, 2), "P5", "a narrower ancestor candidate beats the subclass's";

# An exact tie still goes to the most derived class.
class P6 { multi method m(Int $x) { "P6" } }
class C6 is P6 { multi method m(Int $x) { "C6" } }
is C6.new.m(1), "C6", "an exact tie goes to the subclass";

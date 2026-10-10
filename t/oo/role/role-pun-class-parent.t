use Test;

# A role's `is P` header (P a class) makes P the parent of the role's pun.
plan 9;

class P { method p { 'p' } }
role Pun is P { method q { 'q' } }

my $o = Pun.new;
is $o.^mro.map(*.^name).join(" "), "Pun P Any Mu", "pun .^mro includes the class parent";
is $o.p, 'p', "pun inherits a method from the class parent";
is $o.q, 'q', "pun still has the role's own method";
ok $o ~~ P, "pun instance smartmatches the parent class";
is Pun.new.p, 'p', "a second construction still inherits";

class C does Pun { }
is C.new.p, 'p', "class composing the role inherits the parent";
is C.new.q, 'q', "class composing the role gets the role method";

class P2 { has $.x = 3; method x2 { $!x * 2 } }
role Pun2 is P2 { }
is Pun2.new.x2, 6, "parent attributes are initialised for the pun";
is Pun2.new(x => 5).x2, 10, "named args reach the parent attributes";

use v6;
use Test;

plan 4;

role RM { }

class K does RM {
    multi method bar(Mu:D:) { "mu" }
    multi method bar(RM:D:) { "rm" }
}
is K.new.bar, "rm", "a role-typed invocant beats Mu:D in the same class";

my \m = Metamodel::ClassHOW.new_type: :name<InvocantRankA>;
m.^add_role(RM);
m.^add_multi_method: "foo", my method (Mu:D:) { "mu" };
m.^add_multi_method: "foo", my method (RM:D:) { "rm" };
m.^compose;
is m.new.foo, "rm", "also for candidates added through the MOP";

my \n = Metamodel::ClassHOW.new_type: :name<InvocantRankB>;
n.^add_multi_method: "foo", my method (Mu:D:) { "mu" };
n.^add_multi_method: "foo", my method (RM:D:) { "rm" };
n.^add_role(RM);
n.^compose;
is n.new.foo, "rm", "whichever order the candidates and the role arrive in";

class Plain {
    multi method baz(Mu:D:) { "mu" }
    multi method baz(Plain:D:) { "plain" }
}
is Plain.new.baz, "plain", "a class-typed invocant still beats Mu:D";

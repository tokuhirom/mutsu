use Test;

plan 5;

class Parent { }
role Other { }
role Punned is Parent { }
class Consumer does Punned does Other { }

is Consumer.^roles.map(*.^name).sort.join(" "), "Other Punned",
    ".^roles does not list the class parent of a composed role";
is Consumer.^roles(:!local).map(*.^name).sort.join(" "), "Other Punned",
    ".^roles(:!local) does not list the class parent either";
is Consumer.^mro.map(*.^name).join(" "), "Consumer Parent Any Mu",
    "the MRO still has the parent class";

role Mixed does Other is Parent { }
is Mixed.^roles.map(*.^name).join(" "), "Other",
    "a role's own .^roles keeps the does parent and drops the class parent";
is Punned.^roles.elems, 0, "a role with only a class parent has no roles";

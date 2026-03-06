use Test;

plan 8;

my &subset_ascii   = &infix:<<(<=)>>;
my &superset_ascii = &infix:<<(>=)>>;
my &not_subset     = &infix:<⊈>;
my &not_superset   = &infix:<⊉>;

ok subset_ascii((a => -1).Mix, mix()), "callable (<=) handles negative Mix weights";
ok superset_ascii(mix(), (a => -1).Mix), "callable (>=) handles negative Mix weights";
nok not_subset((a => -1).Mix, mix()), "callable ⊈ negates subset semantics";
nok not_superset(mix(), (a => -1).Mix), "callable ⊉ negates superset semantics";

ok subset_ascii(bag(<a>), bag(<a a>)), "callable (<=) respects Bag multiplicity";
ok not_subset(bag(<a a>), bag(<a>)), "callable ⊈ respects Bag multiplicity";

throws-like { not_subset(1, Failure.new) }, Exception, "⊈ with Failure RHS throws";
throws-like { not_superset(Failure.new, ^3) }, Exception, "⊉ with Failure LHS throws";

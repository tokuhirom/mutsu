use Test;

# A class trait that mixes a role into the class's HOW (`$class.HOW does R`)
# must leave that mixin in place for a class declared as a term
# (`my \c = class C is trait { }`, `given class ... { }`), exactly as for a
# statement-level declaration.

plan 9;

my role Q { method qq(|) { "qq" } }
my role Mark[Mu:U $m] { method marked-with(|) { $m<> } }

multi trait_mod:<is>(Mu:U $c, :$mixsame!) { $c.HOW does Q }
multi trait_mod:<is>(Mu:U $c, Mu:U :$mixparam!) { $c.HOW does Mark[$mixparam] }

class Plain is mixsame { }
is Plain.HOW.^name, 'Perl6::Metamodel::ClassHOW+{Q}', "statement form (baseline)";
is Plain.^qq, "qq", "statement form: the mixed-in method is callable";

my \term = class TermForm is mixsame { };
is term.HOW.^name, 'Perl6::Metamodel::ClassHOW+{Q}', "term form keeps the HOW mixin";
is term.^qq, "qq", "term form: the mixed-in method is callable";

given class Given is mixsame { } {
    is .HOW.^name, 'Perl6::Metamodel::ClassHOW+{Q}', "given class keeps the HOW mixin";
}

my \param = class WithArg is mixparam(Int) { };
is param.^marked-with.^name, "Int", "a parameterised role mixed in by a term-form class";

sub make { class InSub is mixsame { } }
is make().^qq, "qq", "a term-form class declared in a sub body";
is make().^qq, "qq", "and again on the second entry";

my \untouched = class Untouched { };
is untouched.HOW.^name, "Perl6::Metamodel::ClassHOW", "a class without the trait is unaffected";

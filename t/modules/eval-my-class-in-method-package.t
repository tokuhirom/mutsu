use Test;
use MONKEY-SEE-NO-EVAL;

# A `my class` declared by an EVAL run from a method keeps one identity: its
# own methods' `::?CLASS`, its bare name inside and after the body, and a trait
# argument naming it all agree, on every re-evaluation (#17547).

plan 9;

class L {
    method b { EVAL 'my class FB { method n { FB === ::?CLASS } }; FB.n' }
    method d {
        EVAL 'my class FD { has $.x is default(FD); method same { FD === ::?CLASS } };
              (FD.new.x === FD, FD.same, FD.new.x === FD.new.WHAT)'
    }
}

ok L.b, 'the bare name in a method body is ::?CLASS';
ok L.b, 'and again on the next evaluation';
for ^5 -> $i {
    is-deeply L.d, (True, True, True), "trait argument, bare name and ::?CLASS agree (run $i)";
}

my class Plain { method n { Plain.^name } }
is Plain.n, 'Plain', 'a mainline my class is unaffected';
sub s { my class InSub { method n { ::?CLASS.^name } }; InSub.n }
is s(), 'InSub', 'a my class in a mainline sub is unaffected';

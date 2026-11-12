use Test;
use MONKEY-SEE-NO-EVAL;

# A `my class` declared by an EVAL run from a method belongs to the method's
# package, under one identity: its own methods' `::?CLASS`, its bare name
# inside and after the body, and a trait argument naming it all agree, on
# every re-evaluation (#17547).

plan 10;

class L {
    method a { EVAL 'my class FA { }; FA.^name' }
    method b { EVAL 'my class FB { method n { FB.^name } }; FB.n' }
    method c { EVAL 'my class FC { method n { ::?CLASS.^name } }; FC.n' }
    method d {
        EVAL 'my class FD { has $.x is default(FD); method same { FD === ::?CLASS } };
              (FD.new.x === FD, FD.same, FD.^name)'
    }
}

is L.a, 'L::FA', 'the type is named after the method package';
is L.b, 'L::FB', 'the bare name in a method body';
is L.c, 'L::FC', '::?CLASS in a method body';
for ^5 -> $i {
    is-deeply L.d, (True, True, 'L::FD'), "trait argument, bare name and ::?CLASS agree (run $i)";
}

my class Plain { method n { Plain.^name } }
is Plain.n, 'Plain', 'a mainline my class is unaffected';
sub s { my class InSub { method n { ::?CLASS.^name } }; InSub.n }
is s(), 'InSub', 'a my class in a mainline sub is unaffected';

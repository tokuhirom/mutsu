use Test;

# `.^add_role` given a role *declaration expression* (`role :: { ... }`,
# `role Named { ... }`) composes that role. The expression evaluates to the
# individual parametric role (a candidate site key), and `^add_role` resolved
# it by that key instead of the role group, dying with
# "Unknown role: __ANON_ROLE_0__" (#9520).

plan 5;

class D { }
D.^add_role(role :: { method hello { "hi" } });
D.^compose;
is D.new.hello, 'hi', 'an inline anonymous role is composed';

my $r = role :: { method bye { "bye" } };
class C { }
C.^add_role($r);
C.^compose;
is C.new.bye, 'bye', 'an anonymous role held in a variable is composed';
is C.^roles.elems, 1, 'and is listed among the roles';

class E { }
E.^add_role(role NamedAddRole { method n { 5 } });
E.^compose;
is E.new.n, 5, 'a named role declaration expression is composed';

# Red's MetamodelX::Red::Model.compose shape: a custom HOW adding an
# anonymous role to the type it is composing.
class MyHOW is Metamodel::ClassHOW {
    method compose(Mu \type) {
        self.add_role: type, role :: { method greet { "hi from anon role" } }
        nextsame
    }
}
my constant T = MyHOW.new_type(:name<T>);
T.^compose;
is T.new.greet, 'hi from anon role', 'a custom HOW can add an anonymous role in compose';

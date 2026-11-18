use Test;

# An untyped routine parameter is typed `Any`. Whether an instance of a user
# class matches it is now answered once per class while the registry is
# unchanged (#12875). These pin that the memo answers both ways, through
# every call shape that binds such a parameter, and follows a class declared
# after the first answer.

plan 12;

class A { }
class M is Mu { }
class B is A { }

sub f($x) { 'bound' }
class C {
    method m($x) { 'bound' }
}
my $c = C.new;

for ^3 {
    is f(A.new), 'bound', "a sub binds an Any instance (round $_)";
}
throws-like { f(M.new) }, X::TypeCheck::Binding::Parameter,
    'a sub refuses an instance of an `is Mu` class';
throws-like { f(M.new) }, X::TypeCheck::Binding::Parameter,
    'and refuses it again';
is f(B.new), 'bound', 'a subclass instance binds';

is $c.m(A.new), 'bound', 'a method binds an Any instance';
throws-like { $c.m(M.new) }, X::TypeCheck::Binding::Parameter,
    'a method refuses an `is Mu` instance';

my &code = -> $x, $y { 'pointy' };
is code(A.new, B.new), 'pointy', 'a pointy block binds instances';

class Late is Mu { }
throws-like { f(Late.new) }, X::TypeCheck::Binding::Parameter,
    'a class declared later is answered for itself';

is A.new ~~ Any, True, 'smartmatch against Any agrees';
is M.new ~~ Any, False, 'and refuses an `is Mu` instance';

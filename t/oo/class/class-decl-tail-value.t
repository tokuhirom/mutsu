use Test;

# A type declaration is an expression whose value is the type object, also as
# the last statement of a routine, method, block or `do` (CSS::Module's
# `method build { my class builder is CSS::Grammar::AST { … } }`).

plan 6;

class P { method x { 1 } }

sub f { my class B is P { method y { 2 } } }
is f().^name, 'B', 'a `my class` as a sub body tail is the sub value';
is f().x, 1, '... and is the declared class';

sub g { class C2 { } }
is g().^name, 'C2', 'an `our` class as a sub body tail';

my $v = do { my class D { } };
is $v.^name, 'D', 'a class declaration as a `do` block tail';

class Q { method build { my class builder is P { } } }
is Q.build.x, 1, 'a class declaration as a method body tail';

sub r { role RR { } }
ok r() ~~ Positional | RR, 'a role declaration as a sub body tail is the role';

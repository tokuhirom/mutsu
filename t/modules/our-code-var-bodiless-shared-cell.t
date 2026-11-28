use Test;

plan 7;

# A bodiless `our &t` shares one cell with `&S::t`, like `our &t = ...` does, so
# a later class-body statement, a method and a sub body all reach the package
# value through the bare name (#19159).

my $body;
class S { our &t; &t = sub { 3 }; $body = t(); }
is $body, 3, 'a class-body statement assigns and calls a bodiless our &t';

class S2 { our &t; method m { &t = sub { 9 }; t() } }
is S2.m, 9, 'a method assigns the bodiless our &t and calls it';
is &S2::t(), 9, 'the package-qualified name sees the method\'s assignment';

class S3 { our &t; sub f { &t = sub { 5 }; t() }; method run { f() } }
is S3.run, 5, 'a sub body assigns the bodiless our &t';

class S4 { our &t = sub { 1 }; our &t; method m { t() } }
is S4.m, 1, 'a redeclaration keeps the existing package value';

class S5 { our &t; method set { &t = sub { 8 } }; method call { &S5::t() } }
S5.set;
is S5.call, 8, 'a value assigned in one method is seen by another';

# A bodiless `our &name` naming a routine the package already has keeps it.
{
    our sub existing() { 'sub' }
}
{
    our &existing;
    is existing(), 'sub', 'a bodiless our &name does not shadow an our sub';
}

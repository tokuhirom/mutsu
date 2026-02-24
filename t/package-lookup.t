use Test;

plan 7;

class A {
    my $x = 10;
    method x { $A::x = 5; return $A::x; }
    our sub foo() { 'I am foo' };
    method lexical() { $x }
}

isa-ok A::, Stash, 'Typename:: is a Stash';
ok A::<&foo>, 'can access a subroutine in the stash';
ok A:: === A.WHO, 'A:: returns the same as A.WHO';

my $a = A.new;
is $a.x, 5,
    'can access an our-variable in a class package through its long name from class method';
is $a.lexical, 10, 'a lexical of the same name is independent';

lives-ok { my $A::y = 6; $A::y ~~ 6 or die },
    'can declare and access variable in a class package through its long name from outside class';
lives-ok { my $B::x = 7; $B::x ~~ 7 or die },
    'can declare and access variable through its long name without declaring package';

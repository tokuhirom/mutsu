use v6.d;
use Test;

plan 2;

my Int:D $x .= new: 42;
is $x, 42, 'v6.d calls new on the base type';

class A::B::D { }
isa-ok A::B::D.new, A::B::D,
    'a package segment named D is not a definite type smiley';

use Test;

# From Test::Stream: `&Test::Predicator::infix:<!===>` as a hash value.

plan 4;

module Foo {
    our sub infix:<zz>($a, $b) { $a + $b }
    our sub prefix:<qq>($a) { -$a }
}

my $g = &Foo::infix:<zz>;
is $g(1, 2), 3, '&Pkg::infix:<op> names the package operator';
is (&Foo::prefix:<qq>)(5), -5, '&Pkg::prefix:<op>';
my %h = op => &Foo::infix:<zz>, more => 1;
is %h<op>(3, 4), 7, 'inside a hash constructor';
is %h<more>, 1, 'the next pair is not swallowed';

use Test;

plan 5;

# Declaring a variable by an indirect (symbolic) name is rejected at compile time.
throws-like 'my $::("foo")', X::Syntax::Variable::IndirectDeclaration;
throws-like 'my @::("foo")', X::Syntax::Variable::IndirectDeclaration;
throws-like 'my %::("foo")', X::Syntax::Variable::IndirectDeclaration;

# An indirect name as an rvalue (symbolic reference lookup) is still legal.
my $foo = 42;
is $::("foo"), 42, 'indirect rvalue lookup still works';

# A normal declaration is unaffected.
{
    my $bar = 7;
    is $bar, 7, 'ordinary declaration unaffected';
}

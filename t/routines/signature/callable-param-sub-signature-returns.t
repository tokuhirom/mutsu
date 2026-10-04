use Test;

# A callable parameter's signature (`&cmp (Int, Int --> Int)`) keeps its return
# type: upstream NativeCall marshals a C callback's result from it.

plan 3;

sub f(&cmp (Int, Int --> Int)) { }
my $sub = &f.signature.params[0].sub_signature;
ok $sub.returns === Int, '.sub_signature.returns is the declared type';
is $sub.arity, 2, '... with its parameters';
is $sub.raku, ':(Int $, Int $ --> Int)', '... and renders it';

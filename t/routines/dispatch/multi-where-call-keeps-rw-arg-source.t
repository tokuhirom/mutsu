use Test;

# A `where` predicate that calls a method while a multi is dispatched (Red's
# `infix:«>»(Red::AST $a where .returns ~~ Numeric, ...)`) must not consume the
# caller's argument sources: the winning `is rw` candidate still binds to the
# caller's variable, also when the multi is called through a Callable.

plan 6;

class A { method returns { Int } }
multi foo(A $a where .returns ~~ Numeric, A $b) { 'both' }
multi foo(A $a, Int $b is rw) { $b = 42; 'rw' }
multi foo(A $a, Int $b is readonly) { 'ro' }

my $r = 0;
is foo(A.new, $r), 'rw', 'a direct call picks the rw candidate';
is $r, 42, 'and writes the caller variable';

my &c = &foo;
my $s = 0;
is c(A.new, $s), 'rw', 'a call through a Callable picks the rw candidate';
is $s, 42, 'and writes the caller variable';

sub build(&comparison) { -> { my $right = 0; comparison(A.new, $right); $right } }
is build(&foo)(), 42, 'a Callable parameter called on a lexical binds it rw';
is c(A.new, 7), 'ro', 'a literal still picks the readonly candidate';

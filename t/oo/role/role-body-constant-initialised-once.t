use Test;
use lib 't/lib';

# A role body's `my constant` is initialised once, when the role is compiled
# (#16255): not again by the forward-reference shell of a class composing it,
# by each composition or by a runtime mixin. Every expectation was checked
# against rakudo.

plan 11;

my @inits;
role Local {
    my constant TOKEN = do { @inits.push('init'); Mu.new };
    method token { TOKEN.WHICH }
}
is @inits.elems, 1, 'initialised at the declaration, into the static lexical';
class Early does Local { }
class Late does Local { }
is Early.new.token, Late.new.token, 'two composing classes share it';
my %h;
%h does Local;
is %h.token, Late.new.token, 'so does a runtime `does` mixin';
my $o = Mu.new but Local;
is $o.token, Late.new.token, 'and a `but` mixin';
is @inits.elems, 1, 'the initializer ran once';

my @sub-inits;
role Base { my constant BASE = do { @sub-inits.push(1); Mu.new }; method b { BASE.WHICH } }
role Mid does Base { }
class X1 does Mid { }
class X2 does Mid { }
is X1.new.b, X2.new.b, 'a role composed through another role shares it';
is @sub-inits.elems, 1, 'and its initializer ran once';

{
    my role Same { my constant C = 1; method c { C } }
    class A does Same { }
    is A.new.c, 1, 'a lexical role keeps its own constant';
}
{
    my role Same { my constant C = 2; method c { C } }
    class B does Same { }
    is B.new.c, 2, 'and a same-named lexical role elsewhere its own';
}

use RoleConstInitOnce;
my %m;
mk(%m);
is %m.a, %m.b, 'a module role: a method with `use nqp` reads the same constant';
is runs(), 1, 'a module role: the initializer ran once';

use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang declarator that opens a method scope, reads an identifier, type names,
# a signature, colonpairs and a block body (the shape of Qwiratry's `aggsub`,
# ADR-15762 / #13370): the shims `enter-block-scope`, `identifier`, `typename`,
# `colonpair`, `blockoid`, `Nodify` and `replace-signature` all work, a statement
# prefix inside the body is a statement of the body, and the action can DEPARSE
# the body and EVAL core enum values.

plan 13;

use ScopedDeclaratorSlang;

scoped Plain { $.x = 1; }
scoped Typed[Int, Numeric]($v) :!flag {
    NOTED { $.x = 0 }
    $.x = $v;
}

my %plain = scoped-seen('Plain');
is %plain<types>, '', 'no type arguments';
is %plain<signature>, '', 'no signature';
is %plain<body-class>, 'RakuAST::Blockoid', 'the body is a Blockoid';
like %plain<body>, /'$.x = 1'/, 'the body deparses';

my %typed = scoped-seen('Typed');
is %typed<types>, 'Int,Numeric', 'type arguments';
is %typed<signature>, '$v', 'the signature';
is %typed<pairs>, ':!flag', 'the colonpair';
like %typed<body>, /'Nil;'/, 'a statement prefix is a statement of the body';
like %typed<body>, /'$.x = $v'/, 'the rest of the body';
is %typed<order>, 'Order::Same', 'a core enum value is known to an EVAL in a module routine';

role R[::T] { method t { T.^name } }
is R[Mu].new.t, 'Mu', 'a role takes Mu as a type argument';
class C does R[Mu] { }
is C.new.t, 'Mu', 'a class does a role parameterized with Mu';
my $class := Metamodel::ClassHOW.new_type(:name<Composed>);
$class.^add_role(R[Mu]);
lives-ok { $class.^compose }, 'add_role of a Mu-parameterized role composes';

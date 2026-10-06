use v6;
use experimental :rakuast;
use Test;

# A definiteness smiley can follow any name that resolves, so `X:D` over a
# constant naming a type (`constant X = Int`, and so NativeCall's
# `my constant CArray is export`) is a `Type::Definedness` over a
# `Type::Simple` just like `Int:D` -- measured on rakudo 2026.09. The smiley
# must not be refused as an unresolvable bareword because the base is a
# constant rather than a builtin type.

plan 7;

sub expr(Str $src, Int $i = 0) { $src.AST.statements[$i].expression }

my $d = expr(Q|constant X = Int; X:D|, 1);
isa-ok $d, RakuAST::Type::Definedness, '`X:D` over a constant is a Type::Definedness';
ok $d.definite, 'definite';
isa-ok $d.base-type, RakuAST::Type::Simple, 'over a Type::Simple';
is $d.base-type.name.canonicalize, 'X', 'named X';

my $u = expr(Q|constant Y = Int; Y:U|, 1);
isa-ok $u, RakuAST::Type::Definedness, '`Y:U` is a Type::Definedness';
nok $u.definite, 'and not definite';

is Q|constant Z = Int; (3 ~~ Z:D, Int ~~ Z:D, Int ~~ Z:U)|.AST.EVAL.join(','), 'True,False,True',
    'the smiley constrains by the constant\'s value';

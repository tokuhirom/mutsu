use Test;
use MONKEY-SEE-NO-EVAL;

# #19020: the smiley on a parameter's type capture (`::T:D $x`) belongs to the
# `Type::Capture` node, as in rakudo, and the parameter's type stays the
# default `Any`. An `.AST` lowered back keeps the definedness check.

plan 8;

sub param(Str $code) {
    $code.AST.statements[0].expression.signature.parameters[0]
}

is param('sub f(::T:D $x) {}').DEPARSE, '::T:D $x', ':D deparses on the capture';
is param('sub f(::T:U $x) {}').DEPARSE, '::T:U $x', ':U too';
is param('sub f(::T:_ $x) {}').DEPARSE, '::T:_ $x', 'and :_';
is param('sub f(::T Any:D $x) {}').DEPARSE, 'Any:D ::T $x', 'an explicit Any:D stays a type';
is param('sub f(::T:D $x) {}').type.raku, 'RakuAST::Type::Setting.from-identifier("Any")',
    'the parameter type stays the default';
is param('sub f(::T:D $x) {}').type-captures[0].smiley, 'D', 'the capture node holds the smiley';

is EVAL('sub f(::T:D $x) { T.^name }; f(3)'.AST), 'Int',
    'a lowered :D capture binds a definite argument';
dies-ok { EVAL 'sub f(::T:D $x) { T.^name }; f(Int)'.AST }, 'and refuses a type object';

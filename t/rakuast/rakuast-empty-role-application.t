use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 4;

# `R[]` is a parameterized type with no `args` field at all in rakudo.
my $ast = Q[role EmptyApp14604 { }; class EmptyAppC14604 does EmptyApp14604[] { }].AST;
my $trait = $ast.statements.tail.expression.traits[0];
isa-ok $trait.type, RakuAST::Type::Parameterized, 'the does argument is a parameterized type';
is $trait.type.args.args.elems, 0, 'an empty application has no arguments';
unlike $trait.type.gist, /ArgList/, 'no ArgList is rendered for R[]';
is EVAL(Q[role EmptyApp14604b { method x { 7 } }; class EmptyAppC14604b does EmptyApp14604b[] { }; EmptyAppC14604b.new.x].AST),
    7, 'the class composes the role after the round trip';

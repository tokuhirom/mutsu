use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 9;

# `@a>>++` is MetaPostfix::Hyper(Postfix), not a hyper method call (#14845).
my $ast = 'my @a; @a>>++'.AST;
my $apply = $ast.statements[1].expression;
isa-ok $apply, RakuAST::ApplyPostfix, 'ApplyPostfix';
isa-ok $apply.postfix, RakuAST::MetaPostfix::Hyper, 'hyper postfix';
isa-ok $apply.postfix.postfix, RakuAST::Postfix, 'wrapping a Postfix';
is $apply.postfix.postfix.operator, '++', 'operator';

is 'my @a; @a>>++'.AST.DEPARSE.trim, "my @a;\n@a>>++", 'DEPARSE of @a>>++';
is 'my @a; @a>>--'.AST.DEPARSE.trim, "my @a;\n@a>>--", 'DEPARSE of @a>>--';
is 'my @a; @a>>.++'.AST.DEPARSE.trim, "my @a;\n@a>>++", '>>.++ is the same node';
is-deeply EVAL('my @a = 1, 2, 3; @a>>++; @a'.AST), [2, 3, 4], 'EVAL($ast) increments each element';
is-deeply EVAL('my @a = 1, 2, 3; @a>>--; @a'), [0, 1, 2], 'string EVAL agrees';

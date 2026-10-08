use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# A bare mention of a sub the same unit declares (`sub f { }; f`, `f[0]`) is an
# argument-less call: rakudo 2026.09 renders it as
# `Call::Name::WithoutParentheses`, and mutsu's converter used to refuse it as
# an unresolved bareword.

plan 8;

my $src = 'sub niltest { Nil }; niltest[0]';
my $ast = $src.AST;
my $postfix = $ast.statements[1].expression;
isa-ok $postfix, RakuAST::ApplyPostfix, 'a subscript on a bare sub name is a postfix application';
isa-ok $postfix.operand, RakuAST::Call::Name::WithoutParentheses,
    'the bare sub name is an argument-less call';
is $postfix.operand.name.canonicalize, 'niltest', '...of the declared sub';

my $plain = 'sub f { 1 }; f'.AST;
isa-ok $plain.statements[1].expression, RakuAST::Call::Name::WithoutParentheses,
    'a statement that is only the sub name is an argument-less call';

# Round trip: the RakuAST evaluates like the source.
is EVAL('sub g { 42 }; g'.AST), 42, 'EVAL of the round-tripped AST calls the sub';
is EVAL($ast), Nil, 'subscripting the sub result round trips';

# A declared sub does not capture a name that is a type.
my $type = 'sub Int-ish { 1 }; Int'.AST;
isa-ok $type.statements[1].expression, RakuAST::Type::Simple,
    'a builtin type name stays a type next to a sub declaration';
is EVAL('sub h { 7 }; h.succ'.AST), 8, 'a bare sub name as a method invocant';

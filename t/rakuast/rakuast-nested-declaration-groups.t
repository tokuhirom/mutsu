use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 18;

my $assigned = Q[my (\d, (\e, \f)) = 1, (2, 3); d].AST;
my $decl = $assigned.statements[0].expression;
is $decl.^name, 'RakuAST::VarDeclaration::Signature',
    'a nested declaration remains one signature declaration';
is $decl.signature.parameters.elems, 2,
    'the outer group has two parameters rather than three flattened leaves';
is $decl.signature.parameters[1].^name, 'RakuAST::Parameter',
    'the nested group is a parameter';
is $decl.signature.parameters[1].sub-signature.^name, 'RakuAST::Signature',
    'the nested parameter has a sub-signature';
is $decl.signature.parameters[1].sub-signature.parameters.elems, 2,
    'the nested signature has its two source leaves';
is EVAL($assigned), 1, 'a nested assignment declaration round-trips';

my $bound = Q[my (\d, (\e, \f)) := 1, (2, 3); d].AST;
is $bound.statements[0].expression.initializer.^name,
    'RakuAST::Initializer::Bind', 'a nested binding retains its initializer';
is EVAL($bound), 1, 'a nested binding declaration round-trips';

my $deep = Q[my (\d, (\e, (\f, (\g, \h)))) = 1, (2, (3, (4, 5))); d].AST;
my $nested = $deep.statements[0].expression.signature.parameters[1].sub-signature;
$nested = $nested.parameters[1].sub-signature;
$nested = $nested.parameters[1].sub-signature;
is $nested.parameters.elems, 2,
    'nested group boundaries survive through several levels';

my $sequence = Q[my $x = 1, my $y = 2; $x + $y].AST;
is $sequence.statements[0].expression.^name, 'RakuAST::ApplyListInfix',
    'comma-linked declarations form one infix application';
is $sequence.statements[0].expression.operands[1].^name,
    'RakuAST::VarDeclaration::Simple', 'the second declaration stays an operand';
is EVAL($sequence), 3, 'comma-linked declarations round-trip in one lexical scope';

my $loop = Q[loop (my $a = 1, my $b = 2; $a > 0; $a--) { last }; $b].AST;
is $loop.statements[0].setup.^name, 'RakuAST::ApplyListInfix',
    'loop setup retains comma-linked declarations';
is EVAL($loop), 2, 'loop setup declarations round-trip';

my $increment = Q[my $x = 0; my $y = 0; loop (; $x < 2; $x++, $y++) {}; $y].AST;
is $increment.statements[2].increment.^name, 'RakuAST::ApplyListInfix',
    'loop increment retains its comma infix';
is EVAL($increment), 2, 'multiple loop increment expressions round-trip';

my $capture_type = Q[my ::a $a; 1].AST;
is $capture_type.statements[0].expression.type.^name, 'RakuAST::Type::Capture',
    'a declaration type capture remains a Type::Capture';
is EVAL($capture_type), 1, 'a declaration type capture round-trips';

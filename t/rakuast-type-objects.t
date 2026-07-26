use Test;
use experimental :rakuast;

# RakuAST Phase 3 slice 5 (ADR-0011): registered type objects and `.WHAT`.

plan 16;

my $literal = Q[42].AST.statements[0].expression;

is $literal.WHAT.^name, 'RakuAST::IntLiteral', 'node .WHAT has the concrete RakuAST type';
ok $literal.WHAT === RakuAST::IntLiteral, 'node .WHAT is the registered type object';

ok RakuAST::IntLiteral ~~ RakuAST::Node, 'literal type object isa Node';
ok RakuAST::IntLiteral ~~ RakuAST::Expression, 'literal type object isa Expression';
ok RakuAST::IntLiteral ~~ RakuAST::Term, 'literal type object isa Term';

ok RakuAST::Term ~~ RakuAST::Expression, 'Term type object isa Expression';
ok RakuAST::Term ~~ RakuAST::Node, 'Term type object isa Node';
ok RakuAST::Expression ~~ RakuAST::Node, 'Expression type object isa Node';
ok RakuAST::Term::Enum ~~ RakuAST::Expression, 'Term::Enum transitively isa Expression';
ok RakuAST::Term::Whatever ~~ RakuAST::Expression, 'Term::Whatever transitively isa Expression';

ok RakuAST::Statement::If ~~ RakuAST::Statement, 'nested type object isa namespace ancestor';
ok RakuAST::Statement::If ~~ RakuAST::Node, 'statement type object isa Node';
nok RakuAST::Statement::If ~~ RakuAST::Expression, 'statement type object is not an Expression';
nok RakuAST::Name ~~ RakuAST::Expression, 'non-expression node type stays outside Expression';

ok RakuAST::Parameter::Slurpy::Flattened ~~ RakuAST::Parameter::Slurpy,
    'Flattened slurpy type isa its namespace parent';
ok RakuAST::Parameter::Slurpy::Unflattened ~~ RakuAST::Parameter::Slurpy,
    'Unflattened slurpy type isa its namespace parent';

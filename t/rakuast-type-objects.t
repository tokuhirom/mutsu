use Test;
use experimental :rakuast;

# RakuAST Phase 3 slice 5 (ADR-0011): registered type objects and `.WHAT`.

plan 24;

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

ok RakuAST::IntLiteral.isa(RakuAST::Term), 'type-object .isa follows semantic hierarchy';
ok RakuAST::Term.isa(RakuAST::Expression), 'abstract type-object .isa is transitive';
nok RakuAST::Statement::If.isa(RakuAST::Expression),
    'type-object .isa rejects unrelated semantic type';

is RakuAST::IntLiteral.^isa(RakuAST::Term), 1, 'type-object .^isa follows semantic hierarchy';
is RakuAST::Term.^isa(RakuAST::Expression), 1, 'abstract type-object .^isa is transitive';
is RakuAST::Statement::If.^isa(RakuAST::Expression), 0,
    'type-object .^isa rejects unrelated semantic type';

is $literal.^isa(RakuAST::Term), 1, 'node .^isa follows semantic hierarchy';
is $literal.^isa(RakuAST::Statement), 0, 'node .^isa rejects unrelated node type';

use Test;
use experimental :rakuast;

# RakuAST Phase 3 slice 5 (ADR-0011): registered type objects and `.WHAT`.

plan 37;

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

my @literal-mro = RakuAST::IntLiteral.^mro;
is @literal-mro.elems, 6, 'literal type-object MRO has the model hierarchy and roots';
is @literal-mro[0].^name, 'RakuAST::IntLiteral', 'literal MRO starts with concrete type';
is @literal-mro[1].^name, 'RakuAST::Term', 'literal MRO includes semantic Term parent';
is @literal-mro[2].^name, 'RakuAST::Expression', 'literal MRO includes Expression';
is @literal-mro[3].^name, 'RakuAST::Node', 'literal MRO includes Node';
is @literal-mro[4].^name, 'Any', 'literal MRO includes Any';
is @literal-mro[5].^name, 'Mu', 'literal MRO includes Mu';

my @if-parents = RakuAST::Statement::If.^parents;
is @if-parents.elems, 2, 'statement parents omit Any and Mu';
is @if-parents[0].^name, 'RakuAST::Statement', 'statement has namespace parent';
is @if-parents[1].^name, 'RakuAST::Node', 'statement parents include Node';

is RakuAST::IntLiteral.^parents(:local)[0].^name, 'RakuAST::Term',
    ':local reports the immediate semantic parent';
is RakuAST::IntLiteral.^parents(:all)[3].^name, 'Any', ':all retains Any';
is $literal.^mro[0].^name, 'RakuAST::IntLiteral', 'node .^mro uses its concrete model type';

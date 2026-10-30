use experimental :rakuast;
use Test;

# `VarDeclaration::Simple.container-type`, `SemiList.code-statements` and
# `is-resolved` on enum/subset/name terms, read by static::Analyzer (#15610).
# Expected values are rakudo 2026.09's.

plan 8;

my $ast = Q|my Int $x = 1; my @a; my %h; (1; 2);|.AST;
my @decls = $ast.statements.map(*.expression).grep(RakuAST::VarDeclaration::Simple);
is @decls.map(*.container-type.^name).join(','), 'Scalar,Array,Hash', 'the sigil picks the container class';
ok RakuAST::VarDeclaration::Simple.^can('container-type'), 'the class can container-type';

my $semi = $ast.statements[3].expression.semilist;
is $semi.code-statements.elems, 2, 'SemiList.code-statements lists its statements';
ok RakuAST::SemiList.^can('code-statements'), 'the class can code-statements';

ok RakuAST::Term::Enum.^can('is-resolved'), 'Term::Enum can is-resolved';
ok RakuAST::Type::Subset.^can('is-resolved'), 'Type::Subset can is-resolved';
ok RakuAST::Term::Name.^can('is-resolved'), 'Term::Name can is-resolved';
is RakuAST::Term::Name.new(RakuAST::Name.from-identifier('Foo')).is-resolved, False, 'a name built by hand is unresolved';

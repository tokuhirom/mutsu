use Test;

plan 4;

my $source = q:to/END/;
use lib 't/lib';
use DeclareSetRwHow;
my $anon = my frozen :: { has $.a };
END
my $ast = $source.AST;
my $declaration = $ast.statements[*-1];
my $class = $declaration.expression.initializer.expression;

is $declaration.DEPARSE.lines.join('|'), 'my $anon = my frozen :: {|    has $.a|}',
    'a custom declarator keeps its empty name when deparsed';
is $class.gist.contains('RakuAST::Name::Part::EmptyEdge.new'), True,
    'the empty name contains empty-edge parts';
is $class.scope, 'my', 'the lexical scope remains on the class';

my $bare = Q[use lib 't/lib'; use DeclareSetRwHow; my $anon = (frozen :: { });].AST;
is $bare.statements[*-1].DEPARSE, 'my $anon = (anon frozen :: { })',
    'a custom declarator in parentheses keeps its empty name';

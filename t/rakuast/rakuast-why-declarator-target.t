use v6.e.PREVIEW;
use Test;

# `WHY` exists on every node, but only a RakuAST::Doc::DeclaratorTarget has
# docs: any other node answers Nil (measured on rakudo 2026.09). A parsed
# declaration's Doc::Declarator prints only the parts the source wrote.

plan 10;

my $name = RakuAST::Name.from-identifier("f");
is RakuAST::IntLiteral.new(1).WHY.gist, 'Nil', 'a literal has no docs';
is RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(1)).WHY.gist, 'Nil',
    'a statement has no docs';
is $name.WHY.gist, 'Nil', 'a name has no docs';
is RakuAST::StatementList.new.WHY.gist, 'Nil', 'a statement list has no docs';
is RakuAST::Sub.new(name => $name).WHY.^name, 'RakuAST::Doc::Declarator',
    'a sub answers the Doc::Declarator type object';
nok RakuAST::Sub.new(name => $name).WHY.defined, 'which is undefined';

my $ast = Q:to/END/.AST;
#| lead
sub f() { }
sub g() { } #= trail
END
my $lead = $ast.statements[0].expression.WHY;
is $lead.leading.join, 'lead', 'leading docs of a parsed sub';
nok $lead.raku.contains('trailing'), 'an absent trailing part is not printed';
my $trail = $ast.statements[1].expression.WHY;
is $trail.trailing.join, 'trail', 'trailing docs of a parsed sub';
nok $trail.raku.contains('leading'), 'an absent leading part is not printed';

use Test;

# From the Code::Coverage distribution (via Code::Coverable): a statement
# list is walked with `visit-children`, and each statement's line is read
# through `.origin.source.original-line(.origin.from)`.

plan 6;

my $ast = "my \$a = 1;\n\nsay \$a;\n".AST;

my @top;
$ast.visit-children({ @top.push(.^name) });
is-deeply @top, ['RakuAST::Statement::Expression', 'RakuAST::Statement::Expression'],
    'visit-children calls the callable once per direct child';

my @inner;
$ast.statements[1].visit-children({ @inner.push(.^name) });
is-deeply @inner, ['RakuAST::Call::Name::WithoutParentheses'],
    'visit-children descends one level at a time';

my @lines = $ast.statements.map({ .origin.source.original-line(.origin.from) });
is-deeply @lines, [1, 3], 'a statement origin maps back to the line it began on';

is $ast.statements[0].origin.^name, 'RakuAST::Origin', 'origin is a RakuAST::Origin';
ok $ast.statements[0].origin.from.defined, 'origin.from is defined';

my @seen;
sub walk($node) { @seen.push($node.^name); $node.visit-children(&walk) }
$ast.visit-children(&walk);
ok @seen.elems > 2 && @seen.first(* eq 'RakuAST::Statement::Expression'),
    'a recursive walk reaches the nested nodes';

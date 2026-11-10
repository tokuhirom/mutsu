use Test;
use experimental :rakuast;

# A user-declared prefix operator application is `ApplyPrefix` in the RakuAST
# tree (the parser keeps the call of its routine). Identical under rakudo.
plan 5;

my $code = 'sub prefix:<⥷>($x) { "p:$x" }; sub prefix:<ǂ>($x) { "q:$x" }; ⥷ 3; ǂ 4;';
my $ast = $code.AST;
isa-ok $ast.statements[2].expression, RakuAST::ApplyPrefix, 'a glyph prefix is an ApplyPrefix';
is $ast.statements[2].expression.prefix.operator, '⥷', 'with the operator symbol';
isa-ok $ast.statements[2].expression.operand, RakuAST::IntLiteral, 'and the operand';
isa-ok $ast.statements[3].expression, RakuAST::ApplyPrefix, 'a second one';
is $ast.EVAL, 'q:4', 'the tree runs as the call of the operator routine';

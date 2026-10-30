use Test;

plan 8;

# App::RaCoCo t/08-coverable-index.rakutest has `my $a = 1,\n my $b = 2,\n
# say ...;` (a `;` typed as `,`): rakudo reads ONE comma list whose leading
# operands are the declarations and whose tail is the loose expression.
my $src = 'my $b = 2, my $c = 3, say "$b $c";';
my $ast = $src.AST;
my $expression = $ast.statements[0].expression;

is $expression.^name, 'RakuAST::ApplyListInfix', 'one comma list';
is $expression.infix.operator, ',', 'with the comma infix';
is $expression.operands.elems, 3, 'two declarations and the tail';
is $expression.operands.map(*.^name).join(' '),
    'RakuAST::VarDeclaration::Simple RakuAST::VarDeclaration::Simple '
    ~ 'RakuAST::Call::Name::WithoutParentheses',
    'operand classes';

is-deeply (EVAL $src ~ ' $b + $c'), 5, 'the source form runs';

my $out = '';
{
    my $*OUT = class { method print(*@a) { $out ~= @a.join }; method flush {} }.new;
    EVAL $ast;
}
is $out.trim, '2 3', 'EVAL of the converted AST runs the tail';

my $round = $ast.EVAL;
pass 'the AST evaluates twice';

# Two declarations without a tail keep their existing shape.
is 'my $x = 1, my $y = 2;'.AST.statements[0].expression.operands.elems, 2,
    'a comma list of declarations alone';

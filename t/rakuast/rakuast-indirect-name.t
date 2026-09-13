use v6;
use experimental :rakuast;
use Test;

# RakuAST read-direction coverage found through LaTeX::Grammar 0.0.5:
# `::("x")` is a dynamic name lookup, not a static bareword.

plan 4;

my $ast = q[::("x") + 1].AST;

is $ast.^name, 'RakuAST::StatementList',
    'an indirect name participates in the ordinary AST tree';
is $ast.gist, q:to/GIST/.chomp,
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::ApplyInfix.new(
      left  => RakuAST::Term::Name.new(
        RakuAST::Name.new(
          RakuAST::Name::Part::Expression.new(
            RakuAST::QuotedString.new(
              segments   => (
                RakuAST::StrLiteral.new("x"),
              )
            )
          )
        )
      ),
      infix => RakuAST::Infix.new("+"),
      right => RakuAST::IntLiteral.new(1)
    )
  )
)
GIST
    'the dynamic name keeps its expression part in the RakuAST shape';
is $ast.statements[0].expression.left.^name, 'RakuAST::Term::Name',
    'the indirect lookup is represented as a Term::Name';
is EVAL(q[::("Int")].AST).^name, 'Int',
    'the dynamic name also lowers back through EVAL';

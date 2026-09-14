use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: argument-less callable interpolation retains
# Regex::Assertion::Callable while argument-bearing calls remain a separate
# source-tree boundary.

plan 11;

is Q[my sub foo { "bar" }; /<&foo>/].AST.gist, q:to/END/.chomp, 'callable interpolation retains its RakuAST shape';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::Sub.new(
      name => RakuAST::Name.from-identifier("foo"),
      body => RakuAST::Blockoid.new(
        RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::QuotedString.new(
              segments   => (
                RakuAST::StrLiteral.new("bar"),
              )
            )
          )
        )
      )
    )
  ),
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::Assertion::Callable.new(
        callee => RakuAST::Var::Lexical.new("\&foo")
      )
    )
  )
)
END

is Q[my sub foo { "bar" }; /<&foo()>/].AST.gist,
    Q[my sub foo { "bar" }; /<&foo>/].AST.gist,
    'empty callable parentheses have the same RakuAST shape';

my $callable = Q[my sub foo { "bar" }; /<&foo>/].AST
    .statements[1].expression.body;
ok $callable ~~ RakuAST::Regex::Assertion::Callable,
    'the node has its dedicated callable assertion type';
ok $callable ~~ RakuAST::Regex::Atom,
    'a callable assertion is a regex atom';
is $callable.callee.name, '&foo',
    'the callable assertion retains its lexical code variable';
ok $callable.args ~~ RakuAST::ArgList,
    'an omitted argument list answers with its ArgList type object';

my $constructed = RakuAST::Regex::Assertion::Callable.new(
    callee => RakuAST::Var::Lexical.new('&foo'),
);
ok $constructed ~~ RakuAST::Regex::Assertion::Callable,
    'the callable assertion constructor creates the measured node';
is $constructed.gist, q:to/END/.chomp, 'the constructed callable renders like Rakudo';
RakuAST::Regex::Assertion::Callable.new(
  callee => RakuAST::Var::Lexical.new("\&foo")
)
END
is $constructed.callee.name, '&foo',
    'the constructed callable exposes its callee';

my $constructed-regex = EVAL(RakuAST::QuotedRegex.new(body => $constructed));
ok $constructed-regex ~~ Regex,
    'a constructed callable tree lowers through the existing regex value path';

my $with-prefix = Q[my sub foo { "bar" }; /foo <&foo()>/].AST
    .statements[1].expression.body.terms[1];
ok $with-prefix ~~ RakuAST::Regex::Assertion::Callable,
    'callable interpolation remains structural inside a regex sequence';

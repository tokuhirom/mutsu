use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: callable interpolation retains
# Regex::Assertion::Callable, including its argument tree.

plan 25;

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

is Q[my sub foo { "bar" }; /<&foo( )>/].AST.gist,
    Q[my sub foo { "bar" }; /<&foo>/].AST.gist,
    'empty callable parentheses have the same RakuAST shape';

is Q[my sub foo($x) { $x }; /<&foo("bar")>/].AST.gist, q:to/END/.chomp, 'callable interpolation retains a non-empty argument tree';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::Sub.new(
      name      => RakuAST::Name.from-identifier("foo"),
      signature => RakuAST::Signature.new(
        parameters => (
          RakuAST::Parameter.new(
            type     => RakuAST::Type::Setting.new(
              RakuAST::Name.from-identifier("Any")
            ),
            target   => RakuAST::ParameterTarget::Var.new(
              name => "\$x"
            ),
            optional => False
          ),
        )
      ),
      body      => RakuAST::Blockoid.new(
        RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new("\$x")
          )
        )
      )
    )
  ),
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::Assertion::Callable.new(
        callee => RakuAST::Var::Lexical.new("\&foo"),
        args   => RakuAST::ArgList.new(
          RakuAST::QuotedString.new(
            segments   => (
              RakuAST::StrLiteral.new("bar"),
            )
          )
        )
      )
    )
  )
)
END

my $multiple = Q[my sub callabletest($a, $b) { $a }; /<&callabletest(42, "bar")>/].AST.statements[1].expression.body;
is $multiple.args.gist, q:to/END/.chomp, 'multiple callable arguments retain their order';
RakuAST::ArgList.new(
  RakuAST::IntLiteral.new(42),
  RakuAST::QuotedString.new(
    segments   => (
      RakuAST::StrLiteral.new("bar"),
    )
  )
)
END

is Q[my sub foo($x) { $x }; /<&foo: "bar">/].AST.gist,
    Q[my sub foo($x) { $x }; /<&foo("bar")>/].AST.gist,
    'colon callable arguments normalize to the same RakuAST shape';

my $with-args = Q[my sub foo($x) { $x }; /<&foo("bar")>/].AST.statements[1].expression.body;
is $with-args.args.gist, q:to/END/.chomp, 'the callable assertion exposes its argument list';
RakuAST::ArgList.new(
  RakuAST::QuotedString.new(
    segments   => (
      RakuAST::StrLiteral.new("bar"),
    )
  )
)
END

my $callable = Q[my sub foo { "bar" }; /<&foo>/].AST.statements[1].expression.body;
ok $callable ~~ RakuAST::Regex::Assertion::Callable,
    'the node has its dedicated callable assertion type';
ok $callable ~~ RakuAST::Regex::Atom,
    'a callable assertion is a regex atom';
is $callable.callee.name, '&foo',
    'the callable assertion retains its lexical code variable';
ok $callable.args ~~ RakuAST::ArgList,
    'an omitted argument list answers with its ArgList type object';

our regex callabletest($value) { $value };
my $constructed = RakuAST::Regex::Assertion::Callable.new(
    callee => RakuAST::Var::Lexical.new('&callabletest'),
);
ok $constructed ~~ RakuAST::Regex::Assertion::Callable,
    'the callable assertion constructor creates the measured node';
is $constructed.gist, q:to/END/.chomp, 'the constructed callable renders like Rakudo';
RakuAST::Regex::Assertion::Callable.new(
  callee => RakuAST::Var::Lexical.new("\&callabletest")
)
END
is $constructed.callee.name, '&callabletest',
    'the constructed callable exposes its callee';

my $constructed-regex = EVAL(RakuAST::QuotedRegex.new(body => $constructed));
ok $constructed-regex ~~ Regex,
    'a constructed callable tree lowers through the existing regex value path';

my $arg-list = RakuAST::ArgList.new(RakuAST::IntLiteral.new(42));
is $arg-list.gist, q:to/END/.chomp, 'ArgList.new retains positional argument nodes';
RakuAST::ArgList.new(
  RakuAST::IntLiteral.new(42)
)
END

my $constructed-with-args = RakuAST::Regex::Assertion::Callable.new(
    callee => RakuAST::Var::Lexical.new('&callabletest'),
    args   => $arg-list,
);
ok $constructed-with-args ~~ RakuAST::Regex::Assertion::Callable,
    'the callable constructor accepts a non-empty argument list';
is $constructed-with-args.gist, q:to/END/.chomp, 'the constructed callable retains its argument tree';
RakuAST::Regex::Assertion::Callable.new(
  callee => RakuAST::Var::Lexical.new("\&callabletest"),
  args   => RakuAST::ArgList.new(
    RakuAST::IntLiteral.new(42)
  )
)
END
is $constructed-with-args.args.gist, $arg-list.gist,
    'the constructed callable exposes its argument list';
my $constructed-regex-with-args = EVAL(
    RakuAST::QuotedRegex.new(body => $constructed-with-args)
);
ok $constructed-regex-with-args ~~ Regex,
    'a constructed callable with arguments lowers to a regex';
ok '42' ~~ $constructed-regex-with-args,
    'the constructed callable passes its argument to the regex';
ok '41' !~~ $constructed-regex-with-args,
    'the constructed callable rejects a non-matching argument';

ok '42' ~~ /<&callabletest(42)>/,
    'parser-created callable arguments remain executable';
ok '41' !~~ /<&callabletest(42)>/,
    'parser-created callable arguments preserve mismatch behavior';
ok '42' ~~ /<&callabletest: 42>/,
    'colon callable arguments remain executable';

my $with-prefix = Q[my sub foo { "bar" }; /foo <&foo()>/].AST.statements[1].expression.body.terms[1];
ok $with-prefix ~~ RakuAST::Regex::Assertion::Callable,
    'callable interpolation remains structural inside a regex sequence';

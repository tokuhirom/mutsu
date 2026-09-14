use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: plain regex code blocks retain Regex::Block while
# execution continues through ADR-0009's inline matcher.

plan 11;

is Q[/foo { True }/].AST.gist, q:to/END/.chomp, 'a plain code block retains its RakuAST shape';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::Sequence.new(
        RakuAST::Regex::WithWhitespace.new(
          RakuAST::Regex::Literal.new("foo")
        ),
        RakuAST::Regex::Block.new(
          RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
              RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::Term::Enum.from-identifier('True')
                )
              )
            )
          )
        )
      )
    )
  )
)
END

is Q[/foo <?before { True }>/].AST.gist, q:to/END/.chomp, 'a plain code block nested in a lookaround retains its RakuAST shape';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::Sequence.new(
        RakuAST::Regex::WithWhitespace.new(
          RakuAST::Regex::Literal.new("foo")
        ),
        RakuAST::Regex::Assertion::Lookahead.new(
          assertion => RakuAST::Regex::Assertion::Named::RegexArg.new(
            name      => RakuAST::Name.from-identifier("before"),
            regex-arg => RakuAST::Regex::Block.new(
              RakuAST::Block.new(
                body => RakuAST::Blockoid.new(
                  RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                      expression => RakuAST::Term::Enum.from-identifier('True')
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)
END

my $block = Q[/foo { True }/].AST.statements[0].expression.body.terms[1];
ok $block ~~ RakuAST::Regex::Block, 'the block has its dedicated regex type';
ok $block.block ~~ RakuAST::Block, 'the block exposes its nested RakuAST block';
my $constructed-block = RakuAST::Regex::Block.new($block.block);
ok $constructed-block.block ~~ RakuAST::Block,
    'Regex::Block.new accepts a positional RakuAST block';

ok 'foo' ~~ /foo { True }/, 'a plain code block executes and matches';
ok 'foo' ~~ /foo { False }/, 'a plain code block does not become an assertion';
ok 'foo' ~~ /foo <?before { True }>$/, 'a plain code block executes in a lookaround';

my $calls = 0;
my $side-effect = /foo { $calls++; True }/;
ok 'foo' ~~ $side-effect, 'a plain code block preserves inline side effects';
is $calls, 1, 'a plain code block runs once for the real match';

my $constructed = EVAL(RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Sequence.new(
        RakuAST::Regex::Literal.new('foo'),
        $constructed-block,
    ),
));
ok 'foo' ~~ $constructed, 'a constructed Regex::Block uses the existing matcher';

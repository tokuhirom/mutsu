use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: interpolated regex blocks retain
# Regex::Assertion::InterpolatedBlock while execution continues through the
# existing closure-interpolation matcher.

plan 19;

is Q[/foo <{ "bar" }>/].AST.gist, q:to/END/.chomp, 'an interpolated block retains its RakuAST shape';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::Sequence.new(
        RakuAST::Regex::WithWhitespace.new(
          RakuAST::Regex::Literal.new("foo")
        ),
        RakuAST::Regex::Assertion::InterpolatedBlock.new(
          block      => RakuAST::Block.new(
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
          ),
          sequential => False
        )
      )
    )
  )
)
END

my $block = Q[/foo <{ "bar" }>/].AST.statements[0].expression.body.terms[1];
ok $block ~~ RakuAST::Regex::Assertion::InterpolatedBlock,
    'the block has its dedicated interpolated assertion type';
ok $block.block ~~ RakuAST::Block,
    'the interpolated assertion exposes its nested RakuAST block';
is $block.sequential, False,
    'ordinary interpolated blocks are non-sequential';

my $x = 'bar';
my $rx = /foo <{ $x }>/;
ok 'foobar' ~~ $rx, 'a parser-created interpolated block matches its result';
$x = 'baz';
ok 'foobaz' ~~ $rx,
    'an interpolated block reads the current lexical value at match time';
nok 'foobar' ~~ $rx,
    'an interpolated block does not retain a stale lexical value';

my $returned-pattern = /foo <{ 'bar' }>/;
ok 'foobar' ~~ $returned-pattern,
    'an interpolated block executes its body and matches the returned pattern';
ok 'foobar' ~~ /foo <?before <{ 'bar' }>>/,
    'an interpolated block executes inside a lookaround';

my $constructed = EVAL(RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Sequence.new(
        RakuAST::Regex::Literal.new('foo'),
        $block,
    ),
));
ok 'foobar' ~~ $constructed,
    'a constructed interpolated-block tree uses the existing matcher';

my $constructed-block = RakuAST::Regex::Assertion::InterpolatedBlock.new(
    block => $block.block,
    sequential => False,
);
ok $constructed-block ~~ RakuAST::Regex::Assertion::InterpolatedBlock,
    'the constructor accepts a block and sequential field';

my $sequential = Q[/foo || <{ "bar" }>/].AST.statements[0].expression.body;
ok $sequential ~~ RakuAST::Regex::SequentialAlternation,
    'a sequential interpolated block retains the sequential alternation';
my $sequential-block = $sequential.branches[1];
ok $sequential-block ~~ RakuAST::Regex::Assertion::InterpolatedBlock,
    'the sequential branch retains its interpolated-block node';
is $sequential-block.sequential, True,
    'the interpolated block records that it follows a sequential separator';

my $sequential-value = 'bar';
my $sequential-rx = /foo || <{ $sequential-value }>/;
my $sequential-match = 'bar' ~~ $sequential-rx;
ok $sequential-match,
    'a sequential interpolated block matches after the earlier branch fails';
is ~$sequential-match, 'bar',
    'the sequential interpolated branch supplies the matched value';
$sequential-value = 'baz';
ok 'baz' ~~ $sequential-rx,
    'a sequential interpolated block reads its lexical at match time';
nok 'bar' ~~ $sequential-rx,
    'a sequential interpolated block does not retain a stale lexical value';

my $constructed-sequential = EVAL(Q[/foo || <{ $sequential-value }>/].AST);
ok 'baz' ~~ $constructed-sequential,
    'a constructed sequential interpolated tree uses the existing matcher';

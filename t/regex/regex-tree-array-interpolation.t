use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: ordinary array interpolation retains its RakuAST
# shape while execution continues through the match-time runtime parser.

plan 16;

my @parts = <bar baz>;
is Q[/foo @parts/].AST.gist, q:to/END/.chomp, 'an ordinary array interpolation retains its RakuAST shape';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::Sequence.new(
        RakuAST::Regex::WithWhitespace.new(
          RakuAST::Regex::Literal.new("foo")
        ),
        RakuAST::Regex::Interpolation.new(
          sequential => False,
          var        => RakuAST::Var::Lexical.new("\@parts")
        )
      )
    )
  )
)
END

is Q[/foo || @parts/].AST.gist, q:to/END/.chomp, 'a sequential array interpolation retains its sequential field';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::SequentialAlternation.new(
        RakuAST::Regex::Sequence.new(
          RakuAST::Regex::WithWhitespace.new(
            RakuAST::Regex::Literal.new("foo")
          )
        ),
        RakuAST::Regex::Interpolation.new(
          sequential => True,
          var        => RakuAST::Var::Lexical.new("\@parts")
        )
      )
    )
  )
)
END

my $rx = /^foo @parts$/;
ok 'foobar' ~~ $rx, 'a parser-created regex matches the first array element';
ok 'foobaz' ~~ $rx, 'a parser-created regex matches the second array element';
nok 'fooqux' ~~ $rx, 'a parser-created regex rejects a missing array element';
@parts = <qux>;
ok 'fooqux' ~~ $rx, 'a parser-created regex reads array contents at match time';
nok 'foobar' ~~ $rx, 'a parser-created regex does not retain stale array contents';

@parts = <bar baz>;
my $sequential = /foo || @parts/;
ok 'bar' ~~ $sequential,
    'a sequential array interpolation matches after the earlier branch fails';
@parts = <baz>;
ok 'baz' ~~ $sequential,
    'a sequential array interpolation sees later reassignment';
nok 'bar' ~~ $sequential,
    'a sequential array interpolation does not retain stale array contents';

my $interpolation = RakuAST::Regex::Interpolation.new(
    sequential => False,
    var => RakuAST::Var::Lexical.new(q[@parts]),
);
ok $interpolation ~~ RakuAST::Regex::Interpolation,
    'the aggregate interpolation retains its abstract node type';
is $interpolation.var.name, '@parts',
    'the aggregate interpolation exposes its lexical variable';
is $interpolation.sequential, False,
    'the aggregate interpolation defaults to non-sequential';

my $constructed = EVAL(RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Sequence.new(
        RakuAST::Regex::Literal.new('foo'),
        $interpolation,
    ),
));
ok 'foobaz' ~~ $constructed,
    'a constructed array interpolation uses the existing matcher';
@parts = <emu>;
ok 'fooemu' ~~ $constructed,
    'a constructed array interpolation keeps match-time array binding';

my $nested = /foo <?before @parts>/;
ok 'fooemu' ~~ $nested,
    'the ordinary tree slice keeps the existing named-lookaround boundary';

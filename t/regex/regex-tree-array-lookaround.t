use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: array interpolation inside a lookaround retains its
# RakuAST source shape. Execution deliberately stays on the established
# match-time array parser path because the array contents are live after the
# regex is constructed.

plan 13;

my @values = <bar baz>;
is Q[/foo <?@values>/].AST.gist, q:to/END/.chomp, 'a direct array lookaround retains its InterpolatedVar RakuAST node';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::Sequence.new(
        RakuAST::Regex::WithWhitespace.new(
          RakuAST::Regex::Literal.new("foo")
        ),
        RakuAST::Regex::Assertion::Lookahead.new(
          assertion => RakuAST::Regex::Assertion::InterpolatedVar.new(
            sequential => False,
            var        => RakuAST::Var::Lexical.new("\@values")
          )
        )
      )
    )
  )
)
END

my $direct = /foo <?@values>/;
ok 'foobar' ~~ $direct, 'a direct array lookahead matches its current element';
nok 'fooqux' ~~ $direct, 'a direct array lookahead rejects a missing element';
@values = <baz zot>;
ok 'foobaz' ~~ $direct, 'a direct array lookahead sees later reassignment';
ok 'foozot' ~~ $direct, 'a direct array lookahead sees every current element';
nok 'fooqux' ~~ $direct, 'a direct array lookahead rejects the replaced value';

my $negative = /foo <!@values>/;
nok 'foobaz' ~~ $negative, 'a negative array lookahead rejects a current element';
ok 'fooqux' ~~ $negative, 'a negative array lookahead accepts a missing element';

my $interpolated = RakuAST::Regex::Assertion::InterpolatedVar.new(
    sequential => False,
    var => RakuAST::Var::Lexical.new(q[@values]),
);
ok $interpolated ~~ RakuAST::Regex::Assertion,
    'the array lookaround node retains the abstract assertion type';
is $interpolated.sequential, False,
    'the array lookaround interpolation defaults to non-sequential';
is $interpolated.var.name, '@values',
    'the array lookaround exposes its aggregate lexical variable';

my $constructed = EVAL(RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Sequence.new(
        RakuAST::Regex::Literal.new('foo'),
        RakuAST::Regex::Assertion::Lookahead.new(
            assertion => $interpolated,
        ),
    ),
));
ok 'foobaz' ~~ $constructed,
    'a constructed array lookahead lowers through the existing matcher';
@values = <emu>;
ok 'fooemu' ~~ $constructed,
    'a constructed array lookahead keeps match-time array binding';

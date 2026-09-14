use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: a named lookaround's regex argument can retain an
# aggregate interpolation and read the current array at match time.

plan 15;

my @values = <bar baz>;
is Q[/foo <?before @values>/].AST.gist, q:to/END/.chomp, 'a named array lookaround retains its Regex::Interpolation RakuAST node';
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
            regex-arg => RakuAST::Regex::Interpolation.new(
              sequential => False,
              var        => RakuAST::Var::Lexical.new("\@values")
            )
          )
        )
      )
    )
  )
)
END

my $named = /foo <?before @values>/;
ok 'foobar' ~~ $named, 'a named array lookahead matches its current first element';
ok 'foobaz' ~~ $named, 'a named array lookahead matches its current second element';
nok 'fooqux' ~~ $named, 'a named array lookahead rejects a missing element';
@values = <qux>;
ok 'fooqux' ~~ $named, 'a named array lookahead sees later reassignment';
nok 'foobar' ~~ $named, 'a named array lookahead rejects the replaced value';

my $negative = /foo <!before @values>/;
nok 'fooqux' ~~ $negative, 'a negative named array lookahead rejects a current element';
ok 'foobar' ~~ $negative, 'a negative named array lookahead accepts a missing element';
@values = <bar baz>;
nok 'foobaz' ~~ $negative, 'a negative named array lookahead rejects every current element';
ok 'fooqux' ~~ $negative, 'a negative named array lookahead accepts a replaced value';

my $interpolation = RakuAST::Regex::Interpolation.new(
    sequential => False,
    var => RakuAST::Var::Lexical.new(q[@values]),
);
ok $interpolation ~~ RakuAST::Regex,
    'the named regex argument retains the abstract interpolation type';
is $interpolation.sequential, False,
    'the named regex argument interpolation is non-sequential';
is $interpolation.var.name, '@values',
    'the named regex argument exposes its aggregate lexical variable';

my $constructed = EVAL(RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Sequence.new(
        RakuAST::Regex::Literal.new('foo'),
        RakuAST::Regex::Assertion::Lookahead.new(
            assertion => RakuAST::Regex::Assertion::Named::RegexArg.new(
                name => RakuAST::Name.from-identifier('before'),
                regex-arg => $interpolation,
            ),
        ),
    ),
));
ok 'foobaz' ~~ $constructed,
    'a constructed named array lookahead uses the existing matcher';
@values = <emu>;
ok 'fooemu' ~~ $constructed,
    'a constructed named array lookahead keeps match-time array binding';

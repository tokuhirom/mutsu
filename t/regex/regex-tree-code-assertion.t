use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: predicate-block assertions retain their RakuAST shape
# while execution continues through ADR-0009's inline matcher.

plan 14;

is Q[/foo <?{ True }>/].AST.gist, q:to/END/.chomp, 'a positive predicate block retains its RakuAST shape';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::Sequence.new(
        RakuAST::Regex::WithWhitespace.new(
          RakuAST::Regex::Literal.new("foo")
        ),
        RakuAST::Regex::Assertion::PredicateBlock.new(
          block => RakuAST::Block.new(
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

is Q[/foo <!{ False }>/].AST.gist, q:to/END/.chomp, 'a negated predicate block retains its negated field';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::Sequence.new(
        RakuAST::Regex::WithWhitespace.new(
          RakuAST::Regex::Literal.new("foo")
        ),
        RakuAST::Regex::Assertion::PredicateBlock.new(
          negated => True,
          block   => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
              RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::Term::Enum.from-identifier('False')
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

is Q[/foo <?before <?{ True }>>/].AST.gist, q:to/END/.chomp, 'a predicate block nested in a lookaround remains structural';
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
            regex-arg => RakuAST::Regex::Assertion::PredicateBlock.new(
              block => RakuAST::Block.new(
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

my $positive = /foo <?{ True }>/;
ok 'foo' ~~ $positive, 'a positive predicate block succeeds';
ok 'foo' ~~ /foo <!{ False }>/, 'a negated false predicate succeeds';
nok 'foo' ~~ /foo <?{ False }>/, 'a false predicate rejects';
nok 'foo' ~~ /foo <!{ True }>/, 'a negated true predicate rejects';

my $calls = 0;
my $lookaround = /foo <?before <?{ $calls++; True }>>/;
ok 'foo' ~~ $lookaround, 'a predicate block in a lookahead executes inline';
is $calls, 1, 'a lookaround predicate runs once on the real match';

my $predicate = Q[/foo <?{ True }>/].AST.statements[0].expression.body.terms[1];
ok $predicate ~~ RakuAST::Regex::Assertion::PredicateBlock,
    'the predicate has its dedicated assertion type';
is $predicate.negated, False, 'the predicate exposes a false negated field';
ok $predicate.block ~~ RakuAST::Block, 'the predicate exposes its block';

my $constructed = EVAL(RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Sequence.new(
        RakuAST::Regex::Literal.new('foo'),
        $predicate,
    ),
));
ok 'foo' ~~ $constructed, 'a constructed predicate tree uses the existing matcher';

my $negative = Q[/foo <!{ False }>/].AST.statements[0].expression.body;
my $constructed-negative = EVAL(RakuAST::QuotedRegex.new(body => $negative));
ok 'foo' ~~ $constructed-negative,
    'a constructed negated predicate tree preserves execution polarity';

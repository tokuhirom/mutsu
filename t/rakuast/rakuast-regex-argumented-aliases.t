use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: argumented subrule aliases retain their alias and
# argumented assertion tree while execution uses the existing matcher.

plan 28;

is Q[/<alias=word("a")>/].AST.gist, q:to/END/.chomp, 'an argumented subrule alias retains its nested assertion tree';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::Assertion::Alias.new(
        name      => "alias",
        assertion => RakuAST::Regex::Assertion::Named::Args.new(
          name      => RakuAST::Name.from-identifier("word"),
          args      => RakuAST::ArgList.new(
            RakuAST::QuotedString.new(
              segments   => (
                RakuAST::StrLiteral.new("a"),
              )
            )
          ),
          capturing => True
        )
      )
    )
  )
)
END

is Q[/<alias=.word("a")>/].AST.gist, q:to/END/.chomp, 'dot suppression remains on the nested argumented assertion';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::Assertion::Alias.new(
        name      => "alias",
        assertion => RakuAST::Regex::Assertion::Named::Args.new(
          name => RakuAST::Name.from-identifier("word"),
          args => RakuAST::ArgList.new(
            RakuAST::QuotedString.new(
              segments   => (
                RakuAST::StrLiteral.new("a"),
              )
            )
          )
        )
      )
    )
  )
)
END

my $qualified-gist = Q[/<alias=GAlias::word("a")>/].AST.gist;
ok $qualified-gist.contains('Name.from-identifier-parts("GAlias"')
    && $qualified-gist.contains('"word")'),
    'an argumented alias preserves qualified name parts';

my $node = Q[/<alias=word("a")>/].AST.statements[0].expression.body;
is $node.^name, 'RakuAST::Regex::Assertion::Alias',
    'the alias has its dedicated model class';
is $node.name, 'alias', 'the alias name remains accessible';
is $node.assertion.^name, 'RakuAST::Regex::Assertion::Named::Args',
    'the alias assertion retains its argumented model class';
is $node.assertion.name.^name, 'RakuAST::Name',
    'the nested assertion exposes its name node';
is $node.assertion.args.elems, 1,
    'the nested assertion exposes its positional argument';
ok $node.assertion.args.gist.contains('RakuAST::QuotedString'),
    'the nested argument remains a quoted-string node';
is $node.assertion.capturing, True,
    'a positive argumented alias keeps the original capture';

my $constructed-node = RakuAST::Regex::Assertion::Alias.new(
    name => 'alias',
    assertion => RakuAST::Regex::Assertion::Named::Args.new(
        name => RakuAST::Name.from-identifier('word'),
        args => RakuAST::ArgList.new(
            RakuAST::QuotedString.new(
                segments => (RakuAST::StrLiteral.new('a'),)
            )
        ),
        capturing => True,
    ),
);
ok $constructed-node.gist.contains('RakuAST::Regex::Assertion::Named::Args'),
    'the alias constructor accepts an argumented assertion child';

my $dot-node = Q[/<alias=.word("a")>/].AST.statements[0].expression.body;
is $dot-node.assertion.capturing, False,
    'a dot-suppressed argumented alias hides the original capture';
is $dot-node.assertion.args.elems, 1,
    'dot suppression does not discard the argument list';

is Q[/<alias=word: "a">/].AST.gist,
    Q[/<alias=word("a")>/].AST.gist,
    'colon-form argumented aliases normalize to the same model shape';

my $empty-node = Q[/<alias=word()>/].AST.statements[0].expression.body;
is $empty-node.assertion.^name, 'RakuAST::Regex::Assertion::Named::Args',
    'an empty argumented alias retains the argumented assertion class';
is $empty-node.assertion.args.^name, 'RakuAST::ArgList',
    'an empty argumented alias exposes an empty ArgList';
is $empty-node.assertion.args.gist, 'RakuAST::ArgList.new()',
    'the empty argument list keeps its empty model shape';

grammar GAliasRuntime {
    token TOP { <alias=word("a")> }
    token word ($expected) { $expected }
}
my $match = GAliasRuntime.parse('a');
ok $match.defined, 'a parsed argumented alias reaches its target';
is ~$match<alias>, 'a', 'a parsed argumented alias captures under its alias';
is ~$match<word>, 'a', 'a parsed argumented alias keeps the original capture';

grammar GAliasColonRuntime {
    token TOP { <alias=word: "a"> }
    token word ($expected) { $expected }
}
ok GAliasColonRuntime.parse('a').defined,
    'a colon-form argumented alias reaches its target';

grammar GAliasDotRuntime {
    token TOP { <alias=.word("a")> }
    token word ($expected) { $expected }
}
my $dot-match = GAliasDotRuntime.parse('a');
ok $dot-match.defined, 'a dot-suppressed argumented alias reaches its target';
is ~$dot-match<alias>, 'a', 'a dot-suppressed argumented alias captures its alias';
nok $dot-match<word>.defined,
    'a dot-suppressed argumented alias omits the original capture';

my $grammar = EVAL(Q[grammar GAliasConstruct {
    token TOP { <alias=word()> }
    token word { 'a' }
}].AST);
my $constructed-match = $grammar.parse('a');
ok $constructed-match, 'a grammar lowered from RakuAST reaches its argumented alias';
is ~$constructed-match<alias>, 'a',
    'a grammar lowered from RakuAST captures under the alias';
is ~$constructed-match<word>, 'a',
    'a grammar lowered from RakuAST keeps the original capture';
nok $grammar.parse('b').defined,
    'a grammar lowered from RakuAST rejects other text';

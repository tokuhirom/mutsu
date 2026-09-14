use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: argumented subrule assertions retain their ArgList
# source tree while execution continues through the package-aware matcher.

plan 18;

is Q[/<word("a")>/].AST.gist, q:to/END/.chomp, 'an argumented subrule retains its argument tree';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::Assertion::Named::Args.new(
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
END

is Q[/<.word: "a", 2>/].AST.gist, q:to/END/.chomp, 'the dot-suppressed colon form keeps multiple arguments';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::Assertion::Named::Args.new(
        name => RakuAST::Name.from-identifier("word"),
        args => RakuAST::ArgList.new(
          RakuAST::QuotedString.new(
            segments   => (
              RakuAST::StrLiteral.new("a"),
            )
          ),
          RakuAST::IntLiteral.new(2)
        )
      )
    )
  )
)
END

is Q[/<GArg::word("a")>/].AST.gist, q:to/END/.chomp, 'a qualified argumented subrule keeps segmented name parts';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::Assertion::Named::Args.new(
        name      => RakuAST::Name.from-identifier-parts("GArg", "word"),
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
END

my $node = Q[/<word("a")>/].AST.statements[0].expression.body;
is $node.^name, 'RakuAST::Regex::Assertion::Named::Args',
    'the argumented assertion has its dedicated model class';
is $node.name.^name, 'RakuAST::Name', 'the argumented assertion exposes its name';
is $node.args.elems, 1, 'the argumented assertion exposes one argument';
ok $node.args.gist.contains('RakuAST::QuotedString.new'),
    'a quoted argument remains a QuotedString node';
is $node.capturing, True, 'a positive argumented subrule captures';

is Q[/<word()>/].AST.gist, q:to/END/.chomp, 'an empty argument list remains an argumented assertion';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::Assertion::Named::Args.new(
        name      => RakuAST::Name.from-identifier("word"),
        capturing => True
      )
    )
  )
)
END
my $empty-node = Q[/<word()>/].AST.statements[0].expression.body;
is $empty-node.args.^name, 'RakuAST::ArgList',
    'an empty argumented assertion exposes an empty ArgList';
is $empty-node.args.gist, 'RakuAST::ArgList.new()',
    'the empty argument list retains its empty model shape';

grammar GArgRuntime {
    token TOP { <word("a")> }
    token word ($expected) { $expected }
}
ok GArgRuntime.parse('a').defined,
    'a parsed argumented subrule reaches its target';
nok GArgRuntime.parse('b').defined,
    'a parsed argumented subrule rejects other text';

grammar GArgConstruct {
    token word ($expected) { $expected }
}
my $regex = EVAL(Q[/<GArgConstruct::word("a")>/].AST);
ok 'a' ~~ $regex, 'an argumented subrule executes after AST EVAL';
nok 'b' ~~ $regex, 'an AST-EVAL argumented subrule rejects other text';

my $constructed = EVAL(RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Assertion::Named::Args.new(
        name => RakuAST::Name.from-identifier-parts('GArgConstruct', 'word'),
        args => RakuAST::ArgList.new(
            RakuAST::QuotedString.new(
                segments => (RakuAST::StrLiteral.new('a'),)
            )
        ),
        capturing => True,
    ),
));
my $match = 'a' ~~ $constructed;
ok $match, 'a constructed argumented subrule reaches its target';
is ~$match{'GArgConstruct::word'}, 'a',
    'the constructed argumented subrule keeps the original capture';
nok 'b' ~~ $constructed, 'the constructed argumented subrule rejects other text';

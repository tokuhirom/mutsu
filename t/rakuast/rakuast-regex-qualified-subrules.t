use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: qualified regex subrules retain their segmented Name
# shape while matching continues through the existing package-aware matcher.

plan 17;

is Q[/<G::foo>/].AST.gist, q:to/END/.chomp, 'a qualified capturing subrule retains its segmented name';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::Assertion::Named.new(
        name      => RakuAST::Name.from-identifier-parts("G", "foo"),
        capturing => True
      )
    )
  )
)
END

is Q[/<.G::foo>/].AST.gist, q:to/END/.chomp, 'a qualified non-capturing subrule retains its segmented name';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::Assertion::Named.new(
        name => RakuAST::Name.from-identifier-parts("G", "foo")
      )
    )
  )
)
END

is Q[/<alias=G::foo>/].AST.gist, q:to/END/.chomp, 'a short alias may target a qualified subrule';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::Assertion::Alias.new(
        name      => "alias",
        assertion => RakuAST::Regex::Assertion::Named.new(
          name      => RakuAST::Name.from-identifier-parts("G", "foo"),
          capturing => True
        )
      )
    )
  )
)
END

my $node = Q[/<G::foo>/].AST.statements[0].expression.body;
my $name = $node.name;
is $name.parts.elems, 2, 'a qualified subrule exposes two name parts';
is $name.parts[0].^name, 'RakuAST::Name::Part::Simple',
    'qualified name parts use the simple part node';
is $name.parts[0].name, 'G', 'the first qualified name part is accessible';
is $name.parts[1].name, 'foo', 'the final qualified name part is accessible';
is $name.gist, 'RakuAST::Name.from-identifier-parts("G", "foo")',
    'a qualified name renders with the measured constructor';

my $constructed-name = RakuAST::Name.from-identifier-parts('GQualified', 'foo');
is $constructed-name.parts.map(*.name).join('::'), 'GQualified::foo',
    'from-identifier-parts constructs a walkable qualified name';

grammar GQualified {
    token foo { 'a' }
}

my $constructed = EVAL(RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Assertion::Named.new(
        name => $constructed-name,
        capturing => True,
    ),
));
ok 'a' ~~ $constructed, 'a constructed qualified subrule reaches its target';
nok 'b' ~~ $constructed, 'a constructed qualified subrule rejects other text';

my $alias = EVAL(RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Assertion::Alias.new(
        name => 'alias',
        assertion => RakuAST::Regex::Assertion::Named.new(
            name => RakuAST::Name.from-identifier-parts('GQualified', 'foo'),
            capturing => True,
        ),
    ),
));
my $alias-match = 'a' ~~ $alias;
ok $alias-match, 'a constructed alias to a qualified subrule matches';
is ~$alias-match<alias>, 'a', 'the constructed qualified alias captures by alias';
is ~$alias-match{'GQualified::foo'}, 'a',
    'the constructed qualified alias retains the original capture';

my $grammar-ast = Q[grammar GQualifiedAst {
    token foo { 'a' }
    token TOP { <GQualifiedAst::foo> }
}].AST;
ok $grammar-ast.gist.contains('Name.from-identifier-parts("GQualifiedAst", "foo")'),
    'a grammar declaration retains qualified subrule provenance';
my $grammar = EVAL($grammar-ast);
ok $grammar.parse('a'), 'a grammar lowered from RakuAST resolves its qualified subrule';
ok $grammar.parse('a'), 'a second qualified-subrule match keeps the same boundary';

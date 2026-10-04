use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 #8033: a named default in a pointy regex argument survives both
# directions of the RakuAST boundary and still runs at match time.
plan 8;

my $source = Q[/<word(:expected(-> :$candidate = 42 { $candidate == 42 }))>/].AST;
my $gist = $source.gist;
ok $gist.contains('names') && $gist.contains('"candidate"'),
    'the named parameter retains its name';
ok $gist.contains('default => RakuAST::IntLiteral.new(42)'),
    'the named parameter retains its default';
ok EVAL($source) ~~ Regex,
    'a source AST with a named default lowers to a regex';

my $parameter = RakuAST::Parameter.new(
    names => ('candidate',),
    target => RakuAST::ParameterTarget::Var.new(name => '$candidate'),
    default => RakuAST::IntLiteral.new(42),
);
my $statements = RakuAST::StatementList.new;
$statements.add-statement(
    RakuAST::Statement::Expression.new(
        expression => RakuAST::Var::Lexical.new('$candidate'),
    )
);
my $pointy = RakuAST::PointyBlock.new(
    signature => RakuAST::Signature.new(parameters => [$parameter]),
    body => RakuAST::Blockoid.new($statements),
);
my &callable = EVAL($pointy);
is &callable(), 42, 'a constructed pointy block supplies its named default';
is &callable(candidate => 7), 7, 'an explicit named argument overrides the default';

my $pair = RakuAST::ColonPair::Value.new(
    key => 'expected',
    value => RakuAST::Circumfix::Parentheses.new(
        RakuAST::SemiList.new(
            RakuAST::Statement::Expression.new(expression => $pointy),
        ),
    ),
);
my $constructed = RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Assertion::Named::Args.new(
        name => RakuAST::Name.from-identifier('word'),
        args => RakuAST::ArgList.new($pair),
        capturing => True,
    ),
);
ok EVAL($constructed) ~~ Regex,
    'a constructed named-default regex lowers through the matcher';

my $default_value = 42;
grammar GNamedDefaultRegexArgument {
    token TOP { <word(:expected(-> :$candidate = $default_value { $candidate == 42 }))> }
    token word(:$expected) { <.alpha> <?{ $expected() }> }
}
ok GNamedDefaultRegexArgument.parse('a').defined,
    'the named default is evaluated at match time';
$default_value = 41;
ok !GNamedDefaultRegexArgument.parse('a').defined,
    'the named default observes outer lexical reassignment';

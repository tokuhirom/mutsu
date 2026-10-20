use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
plan 7;

my $captured;
multi trait_mod:<is>(Attribute $attribute, :$index!) { $captured = $index }

my $direct = Q[class DirectBracket { has $.x is index[1, 2] }].AST;
my $direct-trait = $direct.statements[0].expression.body.body.statement-list.statements[0].expression.traits[0];
is $direct-trait.name.canonicalize, 'index', 'the trait retains its name';
isa-ok $direct-trait.argument, RakuAST::Circumfix::ArrayComposer,
    'direct brackets are an array-composer argument';
EVAL($direct);
is-deeply $captured, [1, 2], 'the direct bracket argument round-trips through EVAL';

my $parens = Q[class ParenthesizedBracket { has $.x is index([3, 4]) }].AST;
my $paren-trait = $parens.statements[0].expression.body.body.statement-list.statements[0].expression.traits[0];
isa-ok $paren-trait.argument, RakuAST::Circumfix::Parentheses,
    'parentheses around an array remain observable';
EVAL($parens);
is-deeply $captured, [3, 4], 'the parenthesized array round-trips through EVAL';

my $argument = RakuAST::Circumfix::ArrayComposer.new(
    RakuAST::SemiList.new(RakuAST::Statement::Expression.new(
        expression => RakuAST::IntLiteral.new(5))));
my $trait = RakuAST::Trait::Is.new(
    name => RakuAST::Name.from-identifier('index'), argument => $argument);
my $attribute = RakuAST::VarDeclaration::Simple.new(
    scope => 'has', sigil => '$', twigil => '.',
    desigilname => RakuAST::Name.from-identifier('x'), traits => ($trait,));
my $class = RakuAST::Class.new(
    name => RakuAST::Name.from-identifier('ConstructedBracket'),
    body => RakuAST::Block.new(body => RakuAST::Blockoid.new(
        RakuAST::StatementList.new(RakuAST::Statement::Expression.new(expression => $attribute)))));
EVAL(RakuAST::StatementList.new(RakuAST::Statement::Expression.new(expression => $class)));
isa-ok $captured, Array, 'a hand-built bracket argument reaches the trait as an Array';
is-deeply $captured, [5], 'the hand-built argument executes through the declaration plan';

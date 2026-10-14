use Test;

plan 7;

my $name = RakuAST::Name.from-identifier("export");
my $is = RakuAST::Trait::Is.new(name => $name);
isa-ok $is, RakuAST::Trait::Is, 'Trait::Is.new(name => ...) constructs';
is $is.name.canonicalize, 'export', 'name accessor';
nok $is.argument.defined, 'argument is absent by default';
is $is.raku, 'RakuAST::Trait::Is.new(
  name => RakuAST::Name.from-identifier("export")
)', '.raku omits the absent argument';

my $arg = RakuAST::Circumfix::Parentheses.new(
    RakuAST::SemiList.new(
        RakuAST::Statement::Expression.new(
            expression => RakuAST::IntLiteral.new(3))));
my $with = RakuAST::Trait::Is.new(name => RakuAST::Name.from-identifier("foo"), argument => $arg);
isa-ok $with.argument, RakuAST::Circumfix::Parentheses, 'argument accessor';

# Round trip: a parsed trait equals a hand-built one.
my $parsed = Q|sub f is export { 1 }|.AST.statements[0].expression.traits[0];
is $parsed.raku, $is.raku, 'parsed trait matches the hand-built node';

# A hand-built trait on a hand-built class is honoured by EVAL.
my $class = RakuAST::Class.new(
    name => RakuAST::Name.from-identifier('TraitIsNewClass'),
    traits => (RakuAST::Trait::Is.new(name => RakuAST::Name.from-identifier('Exception')),));
EVAL RakuAST::StatementList.new(RakuAST::Statement::Expression.new(expression => $class));
ok ::('TraitIsNewClass').new ~~ Exception, 'EVAL of a class carrying a hand-built is-trait';

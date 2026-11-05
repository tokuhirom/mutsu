use Test;
use experimental :rakuast;

# #16771: the RakuAST constructors that are not schema-driven (ApplyInfix,
# Block, Statement::Expression, Var::Lexical, ColonPair::Value, routines,
# parameters, ...) refuse a named argument the class does not take with the
# bind's own message, as rakudo does. The accepted names come from rakudo's
# `new` signatures, so a name rakudo declares stays accepted.

plan 20;

my $x = RakuAST::Var::Lexical.new('$x');
my $y = RakuAST::Var::Lexical.new('$y');
my $blk = RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new));

sub refused(&code, $what) {
    my $msg = '';
    try { code(); CATCH { default { $msg = .message } } }
    is $msg.lines.head, "Unexpected named argument 'bogus' passed", $what;
}

refused { RakuAST::ApplyInfix.new(left => $x, infix => RakuAST::Infix.new('+'), right => $y, bogus => 1) }, 'ApplyInfix';
refused { RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new), bogus => 1) }, 'Block';
refused { RakuAST::Statement::Expression.new(expression => $x, bogus => 1) }, 'Statement::Expression';
refused { RakuAST::ColonPair::Value.new(key => 'k', value => $x, bogus => 1) }, 'ColonPair::Value';
refused { RakuAST::Statement::If.new(condition => $x, then => $blk, bogus => 1) }, 'Statement::If';
refused { RakuAST::Sub.new(name => RakuAST::Name.from-identifier('f'), bogus => 1) }, 'Sub';
refused { RakuAST::Parameter.new(target => RakuAST::ParameterTarget::Var.new(name => '$a'), bogus => 1) }, 'Parameter';
refused { RakuAST::Call::Name.new(name => RakuAST::Name.from-identifier('f'), bogus => 1) }, 'Call::Name';
refused { RakuAST::VarDeclaration::Simple.new(sigil => '$', desigilname => RakuAST::Name.from-identifier('v'), bogus => 1) }, 'VarDeclaration::Simple';

# the first unknown name is the one reported, wherever it is in the list
my $msg = '';
try { RakuAST::Block.new(zzz => 1, body => RakuAST::Blockoid.new(RakuAST::StatementList.new), bogus => 1); CATCH { default { $msg = .message } } }
is $msg.lines.head, "Unexpected named argument 'zzz' passed", 'the first unknown name is reported';

# every name the class declares is still accepted
lives-ok { RakuAST::ApplyInfix.new(left => $x, infix => RakuAST::Infix.new('+'), right => $y, colonpairs => ()) },
    'ApplyInfix colonpairs';
lives-ok { RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new), implicit-topic => False,
    required-topic => False, exception => False) }, 'Block implicit-topic, required-topic, exception';
lives-ok { RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new), multiness => 'multi') },
    'Block multiness';
lives-ok { RakuAST::Statement::Expression.new(expression => $x, labels => ()) },
    'Statement::Expression labels';
lives-ok { RakuAST::Statement::Expression.new(expression => $x,
    condition-modifier => RakuAST::StatementModifier::If.new($y)) }, 'Statement::Expression condition-modifier';
lives-ok { RakuAST::Statement::Expression.new(expression => $x,
    loop-modifier => RakuAST::StatementModifier::While.new($y)) }, 'Statement::Expression loop-modifier';

# a wrong-typed value is still the constructor's own business, not the name check's
lives-ok { RakuAST::Statement::Expression.new(expression => $x) }, 'a plain call';

# a positional-form constructor is untouched
lives-ok { RakuAST::StrLiteral.new('s') }, 'StrLiteral.new positional';
lives-ok { RakuAST::IntLiteral.new(3) }, 'IntLiteral.new positional';

# the name check is made before the build, so the node is never half-made
my $node;
try { $node = RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new), bogus => 1); CATCH { default { } } }
nok $node.defined, 'a refused constructor builds nothing';

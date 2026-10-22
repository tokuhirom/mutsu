use Test;
use experimental :rakuast;

plan 6;

# `until my $done { ... }`: the declaration is the loop's condition.
my $until = Q|until my $done { $done = True }|.AST;
my $loop = $until.statements.head;
isa-ok $loop, RakuAST::Statement::Loop::Until, 'until is a Loop::Until';
isa-ok $loop.condition, RakuAST::VarDeclaration::Simple, 'its condition is the declaration';
is $loop.condition.desigilname.canonicalize, 'done', 'naming the declared variable';

my $while = Q|while my $go { last }|.AST.statements.head;
isa-ok $while, RakuAST::Statement::Loop::While, 'while is a Loop::While';
isa-ok $while.condition, RakuAST::VarDeclaration::Simple, 'with the declaration as condition';

# The declaration runs once: the body can end the loop through the variable.
is-deeply EVAL(Q|my $i = 0; until my $done { $i++; $done = True if $i >= 3 }; $i|.AST), 3,
    'the declared variable persists across iterations after a round trip';

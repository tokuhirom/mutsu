use Test;

# An unset optional attribute of a loop node answers the attribute's type
# object, `RakuAST::Expression`, as on rakudo 2026.09 (#16467), instead of
# dying "No such method".

plan 9;

use experimental :rakuast;

my $while = RakuAST::Statement::Loop::While.new(
    condition => RakuAST::Var::Lexical.new('$x'),
    body => RakuAST::Block.new);
is $while.setup.^name, 'RakuAST::Expression', 'a while loop has no setup';
is $while.setup.defined, False, 'a type object';
is $while.increment.^name, 'RakuAST::Expression', 'nor an increment';
is $while.condition.^name, 'RakuAST::Var::Lexical', 'its condition is kept';

my $loop = RakuAST::Statement::Loop.new(body => RakuAST::Block.new);
is $loop.setup.^name, 'RakuAST::Expression', 'a bare loop: setup';
is $loop.condition.^name, 'RakuAST::Expression', 'condition';
is $loop.increment.^name, 'RakuAST::Expression', 'increment';

my $until = RakuAST::Statement::Loop::RepeatUntil.new(
    condition => RakuAST::Var::Lexical.new('$x'),
    body => RakuAST::Block.new);
is $until.setup.^name, 'RakuAST::Expression', 'a repeat-until loop: setup';

my $parsed = Q[loop (my $i = 0; $i < 3; $i++) { 1 }].AST.statements[0];
is $parsed.setup.^name, 'RakuAST::VarDeclaration::Simple', 'a parsed loop keeps its setup';

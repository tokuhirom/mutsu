use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# RakuAST::StatementList.new takes its statements positionally (the form
# Rakudo's own `.raku` prints). Tests 1-6 also pass under raku; the last one
# pins mutsu's eager argument check (Rakudo defers the type error).

plan 7;

my $sl = RakuAST::StatementList.new(
    RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(42)));
is $sl.statements.elems, 1, 'positional statement is stored';
is EVAL($sl), 42, 'a positionally constructed StatementList evaluates';

my $two = RakuAST::StatementList.new(
    RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(1)),
    RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(2)));
is $two.statements.elems, 2, 'several positional statements';
is EVAL($two), 2, 'last statement is the value';

$sl.add-statement(
    RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(7)));
is $sl.statements.elems, 2, 'add-statement still works on a populated list';
is EVAL($sl), 7, 'appended statement is evaluated last';

dies-ok { RakuAST::StatementList.new(42) }, 'a non-node argument is rejected';

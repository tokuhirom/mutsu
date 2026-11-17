use experimental :rakuast;
use Test;

# `StatementList.insert-statement($i, $statement)` (#14916) puts a statement in
# front of the `$i`-th one, or after the last for `$i` = the count, in place.
# It answers the list's statements; an index outside 0..count dies (measured on
# rakudo 2026.09).

plan 8;

sub stmt($v) { RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new($v)) }

my $list = RakuAST::StatementList.new(stmt(1));
my $alias = $list;
my $answer = $list.insert-statement(1, stmt(2));
is $answer.elems, 2, 'answers the statements';
isa-ok $answer[1], RakuAST::Statement::Expression, 'the inserted one among them';
$list.insert-statement(0, stmt(0));
is $list.statements.map(*.expression.value), (0, 1, 2), 'inserts in front of the i-th';
is $alias.statements.elems, 3, 'another holder of the list sees it';
$list.insert-statement(2, stmt(9));
is $list.statements.map(*.expression.value), (0, 1, 9, 2), 'inserts in the middle';
throws-like { $list.insert-statement(9, stmt(5)) }, Exception,
    message => 'Cannot insert a statement at 9 in a list of 4 statements',
    'an index past the end dies';
throws-like { $list.insert-statement(-1, stmt(5)) }, Exception,
    message => 'Cannot insert a statement at -1 in a list of 4 statements',
    'a negative index dies';
is EVAL($list), 2, 'EVAL runs the edited list';

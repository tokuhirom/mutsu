use Test;
use experimental :rakuast;

plan 16;

my $s = "sub foo \{ 1 \}".AST.statements[0].expression;
is $s.has-check-time-problems.raku, 'Bool::False', 'a fresh node has no problems';
is $s.sorries.raku, '()', '... no sorries';
is $s.worries.raku, '()', '... and no worries';

$s.add-sorry(X::AdHoc.new(payload => "boom"));
$s.add-worry(X::AdHoc.new(payload => "meh"));
is $s.has-check-time-problems.raku, 'Bool::True', 'a node holding problems says so';
is $s.sorries.elems, 1, 'one sorry';
is $s.worries.elems, 1, 'one worry';
is $s.sorries[0].^name, 'X::AdHoc', 'the exception is kept as it was given';
is $s.sorries[0].message, 'boom', '... with its message';

$s.promote-worries-to-sorries;
is $s.sorries.elems, 2, 'promoting moves the worries to the sorries';
is $s.worries.elems, 0, '... and leaves no worries';

my $t = "sub bar \{ 1 \}".AST.statements[0].expression;
$t.add-sorry(X::AdHoc.new(payload => "s1"));
$t.add-worry(X::AdHoc.new(payload => "w1"));
$t.add-worry(X::AdHoc.new(payload => "w2"));
$t.promote-worries-to-sorries;
is $t.sorries.map(*.message).join(','), 's1,w1,w2', 'promoted worries follow the sorries in order';
$t.clear-worries;
is $t.has-check-time-problems.raku, 'Bool::True', 'clear-worries leaves the sorries';

my $u = RakuAST::IntLiteral.new(3);
$u.add-worry(X::AdHoc.new(payload => "x"));
is $u.worries.elems, 1, 'a hand-built node collects problems too';
is $u.raku, 'RakuAST::IntLiteral.new(3)', '.raku does not show them';
$u.clear-worries;
is $u.worries.elems, 0, 'clear-worries empties the worries';
is $u.has-check-time-problems.raku, 'Bool::False', '... and the node has no problems again';

use Test;

# `react`, `whenever` and `done` in RakuAST, measured on rakudo 2026.09.

plan 13;

sub react-node($src) { $src.AST.statements.head.expression }

my $r = react-node(Q[react { whenever Supply.from-list(1) -> $v { say $v } }]);
isa-ok $r, RakuAST::StatementPrefix::React, '`react { … }` is a StatementPrefix::React';
isa-ok $r.blorst, RakuAST::Block, 'around its block';
my $w = $r.blorst.body.statement-list.statements[0];
isa-ok $w, RakuAST::Statement::Whenever, '`whenever` is a Statement::Whenever';
isa-ok $w.trigger, RakuAST::ApplyPostfix, 'whose trigger is the supply expression';
isa-ok $w.body, RakuAST::PointyBlock, 'a pointy body is a PointyBlock';

my $bare-react = react-node(Q[react { whenever Supply.from-list(1) { say $_ } }]);
my $bare = $bare-react.blorst.body.statement-list.statements[0].body;
isa-ok $bare, RakuAST::Block, 'a bare body is a Block';
is-deeply ($bare.implicit-topic, $bare.required-topic), (True, True), 'that takes the topic';

isa-ok react-node(Q[react whenever Supply.from-list(1) { say $_ }]).blorst,
    RakuAST::Statement::Whenever, '`react whenever …` holds the whenever directly';

my $done-react = react-node(Q[react { whenever Supply.from-list(1) { done } }]);
my $done-whenever = $done-react.blorst.body.statement-list.statements[0];
my $done = $done-whenever.body.body.statement-list.statements[0].expression;
isa-ok $done, RakuAST::Call::Name::WithoutParentheses, '`done` is a bare call';

is EVAL(Q[my @g; react { whenever Supply.from-list(1,2,3) -> $v { @g.push($v); done if $v == 2 } }; @g.join(',')].AST),
    '1,2', 'a pointy whenever and `done` survive the round trip';
is EVAL(Q[my @g; react whenever Supply.from-list('a','b') { @g.push($_) }; @g.join(',')].AST),
    'a,b', 'the statement form survives it';
is EVAL(Q[my @g; react { whenever Supply.from-list(1,2) -> $x { @g.push($x) }; whenever Supply.from-list(10) { @g.push($_) } }; @g.sort.join(',')].AST),
    '1,2,10', 'several whenevers survive it';
is EVAL(Q[my $s = 0; react { whenever Supply.from-list(1,2) -> Int $n { $s += $n } }; $s].AST),
    3, 'a typed pointy parameter survives it';

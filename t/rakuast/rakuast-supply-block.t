use Test;

# `supply { … }` in RakuAST, measured on rakudo 2026.09. mutsu's parser expands
# the block into an on-demand supply; the expansion keeps the written body as
# a source-form record, which `.AST` renders and EVAL hands back.

plan 13;

my $init = Q[my $s = supply { emit 1; whenever Supply.from-list(2) { emit $_; done } }]
    .AST.statements.head.expression.initializer.expression;
isa-ok $init, RakuAST::StatementPrefix::Supply, '`supply { … }` is a StatementPrefix::Supply';
isa-ok $init.blorst, RakuAST::Block, 'around its block';
my @statements = $init.blorst.body.statement-list.statements;
is @statements.elems, 2, 'holding the written statements';
isa-ok @statements[1], RakuAST::Statement::Whenever, 'a whenever inside it stays a whenever';
nok $init.gist.contains('on-demand'), 'the expansion does not leak into the node';

is EVAL(Q[my $s = supply { emit 1; whenever Supply.from-list(2,3) { emit $_ * 10; done if $_ == 3 } }; $s.list.join(',')].AST),
    '1,20,30', 'emit, whenever and done survive the round trip';
is EVAL(Q[my $t = supply { for ^3 { emit $_ } }; $t.list.join(',')].AST),
    '0,1,2', 'an emit inside a loop survives it';
is EVAL(Q[sub mk($n) { supply { emit $n; emit $n + 1 } }; mk(5).list.join(',')].AST),
    '5,6', 'a supply closing over a parameter survives it';
is EVAL(Q[my @g; my $u = supply { whenever Supply.from-list(1,2) -> $v { emit $v * 2 } }; react { whenever $u { @g.push($_) } }; @g.join(',')].AST),
    '2,4', 'a supply tapped by react survives it';

my $statement = Q[my $s = supply emit 42].AST.statements.head.expression.initializer.expression;
isa-ok $statement, RakuAST::StatementPrefix::Supply,
    'the statement-prefix form is represented as a supply source block';
is EVAL(Q[my $s = supply emit 42; $s.list.join(',')].AST),
    '42', 'a statement-prefix supply emits after the round trip';

my $whenever = Q[my $s = supply whenever Supply.from-list(6,7) -> $v { emit $v * 2 }]
    .AST.statements.head.expression.initializer.expression;
isa-ok $whenever, RakuAST::StatementPrefix::Supply,
    'supply whenever shorthand is represented as a supply source block';
is EVAL(Q[my $s = supply whenever Supply.from-list(6,7) -> $v { emit $v * 2 }; $s.list.join(',')].AST),
    '12,14', 'supply whenever shorthand survives the round trip';

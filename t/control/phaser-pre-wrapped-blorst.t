use Test;
use experimental :rakuast;

plan 8;

# `PRE { COND }` is wrapped by rakudo into
# `X::Phaser::PrePost.new(condition => "SRC").throw unless COND`;
# `.blorst` is that wrapper, which `.DEPARSE` reads the condition back from (#14845).
my $pre = 'PRE { 1 }'.AST.find-nodes(RakuAST::StatementPrefix::Phaser::Pre)[0];
my $wrapped = $pre.blorst;
isa-ok $wrapped, RakuAST::Statement::Expression, 'blorst is a statement';
isa-ok $wrapped.condition-modifier, RakuAST::StatementModifier::Unless, 'the condition is an unless modifier';
isa-ok $wrapped.condition-modifier.expression, RakuAST::ApplyPostfix, 'it calls the written block';
is $wrapped.expression.postfix.name.canonicalize, 'throw', 'the statement throws';
is $wrapped.expression.operand.postfix.args.args[0].value.value, '{ 1 }', 'carrying the condition source';

is 'sub f($x) { PRE { $x > 0 }; $x }'.AST.DEPARSE.trim,
   "sub f (\$x) \{\n    PRE \{\n        \$x > 0\n    };\n    \$x\n}", 'DEPARSE of PRE in a sub';
is 'PRE 1'.AST.DEPARSE.trim, 'PRE 1', 'the bare form deparses';
is 'POST { 2 }'.AST.DEPARSE.trim, "POST \{\n    2\n}", 'POST deparses';

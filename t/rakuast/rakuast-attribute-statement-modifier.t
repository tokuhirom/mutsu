use v6;
use experimental :rakuast;
use Test;

plan 11;

sub attr-statement($source) {
    $source.AST.statements[0].expression.body.body.statement-list.statements[0]
}

my $when = attr-statement(Q[class AttributeWhen { has $.x is required when !*.DEFINITE }]);
isa-ok $when.expression, RakuAST::VarDeclaration::Simple,
    'an attribute modifier keeps the attribute declaration as the expression';
isa-ok $when.condition-modifier, RakuAST::StatementModifier::When,
    'the written `when` remains a condition modifier';
is $when.expression.scope, 'has', 'the declaration keeps its attribute scope';

my $if = attr-statement(Q[class AttributeIf { has $.x = 5 if False }]);
isa-ok $if.condition-modifier, RakuAST::StatementModifier::If,
    'an `if` modifier remains on the declaration';
is $if.expression.initializer.expression.value, 5,
    'the initializer stays attached to the declaration';
isa-ok attr-statement(Q[class AttributeWith { has $.x with 3 }]).condition-modifier,
    RakuAST::StatementModifier::With, 'a `with` modifier keeps its source node';
isa-ok attr-statement(Q[class AttributeUnless { has $.x unless False }]).condition-modifier,
    RakuAST::StatementModifier::Unless, 'an `unless` modifier keeps its source node';
isa-ok attr-statement(Q[class AttributeWithout { has $.x without Nil }]).condition-modifier,
    RakuAST::StatementModifier::Without, 'a `without` modifier keeps its source node';

is EVAL(Q[class AttributeRequired { has $.x is required when !*.DEFINITE }; AttributeRequired.new(x => 7).x].AST),
    7, 'EVAL keeps an attribute declaration unconditional';
is EVAL(Q[my $counter = 0; class AttributeSideEffect { has $.x = 3 if $counter++ }; $counter].AST),
    1, 'the condition runs exactly once when the class body is composed';
is EVAL(Q[class AttributeWithRuntime { has $.x = 9 with 3 }; AttributeWithRuntime.new.x].AST),
    9, 'a topicalizing modifier does not gate the attribute default';

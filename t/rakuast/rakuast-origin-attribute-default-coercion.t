use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: an attribute's literal
# default (`has Str:D $.s = "x"`: the `Initializer::Assign` and its value), and
# a coercion type (`Str(Int)`, `Str()`) of a declaration or a parameter: the
# base type spans the whole coercion, its `Name` the name before the `(`, the
# constraint the text between the parentheses.

plan 3;

sub o($n) { $n.origin ?? $n.origin.from ~ '-' ~ $n.origin.to !! '-' }

my $d = Q[class A { has Str:D $.s = "x"; }].AST.statements[0].expression.body.body.statement-list.statements[0].expression;
is o($d.initializer) ~ ' ' ~ o($d.initializer.expression), '24-29 26-29', 'an attribute default';

my $t = Q[my Str(Int) $x;].AST.statements[0].expression.type;
is (o($t), o($t.base-type), o($t.base-type.name), o($t.constraint), o($t.constraint.name)).join(' '),
    '3-11 3-11 3-6 7-10 7-10', 'a coercion type of a declaration';

$t = Q[sub f(Str() $c) {}].AST.statements[0].expression.signature.parameters[0].type;
is (o($t), o($t.base-type), o($t.base-type.name)).join(' '), '6-11 6-11 6-9',
    'a coercion type of a parameter';

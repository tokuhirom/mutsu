use Test;

# `has $!g OP= EXPR` in RakuAST, measured on rakudo 2026.09: the declaration
# is the left operand of an `ApplyInfix` whose infix is
# `MetaInfix::Assign(Infix(OP))`, exactly as for `my $x OP= EXPR`. The
# declaration itself has no initializer, so EVAL of the tree installs the
# attribute on the class wherever the `has` sits.

plan 9;

sub method-stmt($src) {
    my $method = $src.AST.statements[0].expression.body.body.statement-list.statements[0].expression;
    $method.body.statement-list.statements[0].expression
}

# --- the tree
{
    my $a = method-stmt(Q[class HCA1 { method m { has $!g //= 5; $!g } }]);
    isa-ok $a, RakuAST::ApplyInfix, '`has $!g //= 5` is an ApplyInfix';
    isa-ok $a.left, RakuAST::VarDeclaration::Simple, '... over the declaration';
    is $a.left.scope, 'has', '... of an attribute';
    nok $a.left.initializer.defined, '... which has no initializer';
    isa-ok $a.infix, RakuAST::MetaInfix::Assign, '... with an assignment metaoperator';
    is $a.infix.infix.operator, '//', '... over `//`';
    isa-ok $a.right, RakuAST::IntLiteral, '... and the written right-hand side';
}

# --- EVAL of the tree installs the attribute
is Q[class HCA2 { method m { has $!g ||= 5; 1 } }; HCA2.^attributes.map(*.name).join(',')].AST.EVAL,
    '$!g', 'the attribute is declared on the class';
is Q[class HCA3 { has $!n = 1; method m { has $!g += 2; 1 } }; HCA3.^attributes.elems].AST.EVAL,
    2, '... next to the class-level ones';

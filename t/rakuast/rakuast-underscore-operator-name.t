use Test;

# A user operator whose name starts with `__` (`infix:<__>`) in RakuAST,
# measured on rakudo 2026.09: it is an ordinary `ApplyInfix` over
# `Infix("__")`, and `[__]` an ordinary `Term::Reduce`. Only the parser's own
# vehicles (`is`, `§`) are not operators; a `__` prefix is a legal name.

plan 7;

{
    my $ast = Q[sub infix:<__>($a, $b) { "$a $b" }; "a" __ "b"; [__] 1, 2, 3].AST;
    my $app = $ast.statements[1].expression;
    isa-ok $app, RakuAST::ApplyInfix, '`"a" __ "b"` is an ApplyInfix';
    is $app.infix.operator, '__', 'over the declared `__` operator';
    isa-ok $ast.statements[2].expression, RakuAST::Term::Reduce, '`[__]` is a Term::Reduce';
}

is Q[sub infix:<__>($a, $b) { "$a $b" }; "a" __ "b"].AST.EVAL, 'a b',
    'EVAL of the tree applies the operator';
is Q[sub infix:<__x>($a, $b) { $a ~ $b }; [__x] 'p', 'q', 'r'].AST.EVAL, 'pqr',
    'EVAL of the tree reduces over the operator';

{
    sub infix:<__>($a, $b) { "$a-$b" }
    is 1 __ 2, '1-2', 'the parsed expression applies the operator';
    is ([__] 1, 2, 3), '1-2-3', 'the parsed reduction folds over it';
}

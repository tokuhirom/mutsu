use Test;

# A sigilless term bound to a declarator in RakuAST, measured on rakudo
# 2026.09: `my \b = my $x = 3` is a `VarDeclaration::Term` whose initializer
# holds the inner `VarDeclaration::Simple` itself, and `my \d = my $ = 4` the
# inner `VarDeclaration::Anonymous`. A `my $ = EXPR` in expression position
# is a `VarDeclaration::Anonymous` with the default scope (no `scope`).

plan 13;

sub stmt($src, $i = 0) { $src.AST.statements[$i].expression }

# --- the tree
{
    my $t = stmt(Q[my \b = my $x = 3]);
    isa-ok $t, RakuAST::VarDeclaration::Term, '`my \b = my $x = 3` is a VarDeclaration::Term';
    my $inner = $t.initializer.expression;
    isa-ok $inner, RakuAST::VarDeclaration::Simple, '... over the inner VarDeclaration::Simple';
    is $inner.name, '$x', '... of $x';
}
{
    my $inner = stmt(Q[my \d = my $ = 4]).initializer.expression;
    isa-ok $inner, RakuAST::VarDeclaration::Anonymous, '`my \d = my $ = 4` holds a VarDeclaration::Anonymous';
    is $inner.sigil, '$', '... with the `$` sigil';
    isa-ok $inner.initializer.expression, RakuAST::IntLiteral, '... and its own initializer';
}
{
    my $inner = stmt(Q[my \c = my $]).initializer.expression;
    isa-ok $inner, RakuAST::VarDeclaration::Anonymous, '`my \c = my $` holds an uninitialized Anonymous';
}
isa-ok stmt(Q[say(my $ = -3)]).args.args[0], RakuAST::VarDeclaration::Anonymous,
    '`my $ = -3` as an argument is a VarDeclaration::Anonymous';

# --- EVAL of the tree
is Q[my \b = my $x = 3; b = 9; $x].AST.EVAL, 9, 'the term aliases the declared variable';
is Q[my \d = my $ = 4; d = 5; d].AST.EVAL, 5, 'the term is bound to the anonymous Scalar';
is Q[my \p = my $ = 1; my \q = my $ = 2; p = 10; q].AST.EVAL, 2,
    'two anonymous declarators are separate containers';

# --- semantics, as parsed
{
    my \foo = my $y = 3;
    foo = 7;
    is $y, 7, 'the parsed term writes through to the declared variable';
    my \bar = my $ = -3;
    bar .= abs;
    is bar, 3, 'the parsed term writes through the anonymous Scalar';
}

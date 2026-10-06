use Test;

# The anonymous variables in RakuAST, measured on rakudo 2026.09. A bare `$`
# (and `@`, `%`) is a `state` variable of the block it is written in, and
# rakudo has a node for it with no name: `VarDeclaration::Anonymous(scope =>
# "state", sigil)`, with an `initializer` for `state $ = 0`. mutsu's parser
# mints a name for each occurrence and declares it at the top of the enclosing
# block; the tree shows only the node, and the lowering mints and declares them
# again, with the per-call spelling below a routine body, so the counters
# behave as the parsed program's.

plan 29;

sub exprs($src) { $src.AST.statements.map(*.expression) }
sub body-exprs($src) { $src.AST.statements[0].expression.body.statement-list.statements.map(*.expression) }
sub same($src, $expected, $desc) {
    my $parsed = EVAL($src);
    my $round = EVAL($src.AST);
    is "$parsed|$round", "$expected|$expected", $desc;
}

# --- the tree
{
    my @e = body-exprs(Q[sub f { $++ }]);
    is @e.elems, 1, 'the implicit `state` declaration is not a statement';
    isa-ok @e[0], RakuAST::ApplyPostfix, '`$++` is a postfix application';
    isa-ok @e[0].operand, RakuAST::VarDeclaration::Anonymous, 'over an anonymous declaration';
    is @e[0].operand.scope, 'state', 'a state variable';
    is @e[0].operand.sigil, '$', 'a scalar';
    is @e[0].postfix.operator, '++', 'with the postfix operator';
    my @p = body-exprs(Q[sub f { ++$ }]);
    isa-ok @p[0], RakuAST::ApplyPrefix, '`++$` is a prefix application';
    isa-ok @p[0].operand, RakuAST::VarDeclaration::Anonymous, 'over an anonymous declaration';
    my @s = body-exprs(Q[sub f { $++ + $++ }]);
    isa-ok @s[0].left.operand, RakuAST::VarDeclaration::Anonymous, 'each occurrence is its own node';
    isa-ok @s[0].right.operand, RakuAST::VarDeclaration::Anonymous, 'twice';
    my @a = body-exprs(Q[sub g { $ = 5 }]);
    isa-ok @a[0], RakuAST::ApplyInfix, '`$ = 5` is an assignment';
    isa-ok @a[0].left, RakuAST::VarDeclaration::Anonymous, 'to an anonymous declaration';
    my @t = exprs(Q[state $ = 0; state $]);
    isa-ok @t[0], RakuAST::VarDeclaration::Anonymous, '`state $ = 0` is an anonymous declaration';
    isa-ok @t[0].initializer, RakuAST::Initializer::Assign, 'with its initializer';
    isa-ok @t[1], RakuAST::VarDeclaration::Anonymous, '`state $` is one';
    nok @t[1].initializer.defined, 'with none';
    my @n = exprs(Q[my $x = (state $ = 3)]);
    like @n[0].gist, /'Circumfix::Parentheses' .* 'VarDeclaration::Anonymous'/,
        'as the initializer of another declaration';
    my @b = body-exprs(Q[sub t { @ }]);
    is @b[0].sigil, '@', 'an anonymous array';
    my @h = body-exprs(Q[sub t { % }]);
    is @h[0].sigil, '%', 'an anonymous hash';
}

# --- the round trip counts like the parsed program
same Q[sub f { $++ }; f(); f(); f()], 2, '`$++` persists across calls of a routine';
same Q[sub f { ++$ }; f(); f(); f()], 3, 'and `++$`';
same Q[sub f { $++ + $++ }; f(); f(); f()], 4, 'two of them count separately';
same Q[sub f { state $n = 5; $n++ }; f(); f(); f()], 7, 'a named state variable still works';
same Q[sub f { $ = 5 }; f()], 5, 'an assignment to `$`';
same Q[my @a; for 1..3 { @a.push: $++ }; @a.join(",")], '0,1,2', 'a loop body shares it between iterations';
same Q[my @r; for ^2 { @r.push: (^3).map({ ++$ }).join(",") }; @r.join("|")], '1,2,3|1,2,3',
    'a block cloned per iteration restarts it';
same Q[sub h { (^2).map({ $++ }).join(",") }; h(); h()], '0,1', 'a block below a routine restarts per call';
same Q[sub g { my @r; for ^3 { @r.push: $++ }; @r.join(",") }; g(); g()], '0,1,2', 'also in a loop below a routine';
same Q[my $x = (state $ = 3); $x], 3, 'a state initializer as an expression';

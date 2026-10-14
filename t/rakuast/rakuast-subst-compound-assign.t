use Test;

# `s[PATTERN] OP= EXPR` in RakuAST, measured on rakudo 2026.09: a
# `Substitution` whose `infix` is `MetaInfix::Assign(Infix(OP))` and whose
# `replacement` is EXPR itself (`s[...] = EXPR` has `infix => Assignment`).
# Each match is replaced by `$/ OP EXPR`, and the expression answers the
# Match (a List of them under `:g`), as `s[...] = EXPR` does.

plan 20;

sub subst($src) { $src.AST.statements.head.expression }

# --- the tree
{
    my $s = subst(Q[s[\d] += 5]);
    isa-ok $s, RakuAST::Substitution, '`s[\d] += 5` is a Substitution';
    isa-ok $s.infix, RakuAST::MetaInfix::Assign, 'its infix is MetaInfix::Assign';
    is $s.infix.infix.operator, '+', 'over the written operator';
    isa-ok $s.replacement, RakuAST::IntLiteral, 'the replacement is the written expression';
    nok $s.immutable, 'it is destructive';
}
{
    my $s = subst(Q[s:g[\w] x= 2]);
    is $s.infix.infix.operator, 'x', 'a word operator';
    is $s.adverbs.map(*.key).join(','), 'g', 'the adverb stays an adverb';
}
isa-ok subst(Q[s[\d] = $/ + 1]).infix, RakuAST::Assignment, '`s[...] = EXPR` keeps a plain Assignment';

# --- EVAL of the tree
# (rakudo 2026.09 itself mis-evaluates these trees: its replacement thunk
# stringifies a Code object, "Code object coerced to string".)
$_ = 'a 2 3';
is Q[s[\d] += 5; $_].AST.EVAL, 'a 7 3', 'EVAL of `s[\d] += 5` adds to the matched digit';
$_ = 'a b c';
is Q[s:g[\w] x= 2; $_].AST.EVAL, 'aa bb cc', 'EVAL of `s:g[\w] x= 2` repeats every match';
$_ = 'ab';
is Q[s[(a)] ~= $0; $_].AST.EVAL, 'aab', 'the replacement sees the match captures';

# --- semantics, as parsed
{
    $_ = 'a 2';
    my $r = (s[\d] += 5);
    isa-ok $r, Match, 'the expression answers the Match';
    is $r.Str, '2', '... of the replaced text';
    is $_, 'a 7', 'the topic is changed';
}
{
    $_ = 'a b';
    my $r = (s:g[\w] x= 2);
    is $r.elems, 2, 'under :g it answers every Match';
    is $_, 'aa bb', 'and replaces each of them';
}
{
    $_ = 'a 2 3';
    s:g[\d] -= 1;
    is $_, 'a 1 2', '`-=` under :g';
}
{
    $_ = 'x1';
    s:i[X] ~= 'y';
    is $_, 'xy1', 'an inline adverb applies to the pattern';
}
{
    sub infix:<plus2>($a, $b) { $a + $b + 2 }
    $_ = '5';
    s[\d] plus2= 1;
    is $_, '8', 'a user operator';
}
{
    my $t = 'a 4';
    given $t -> $_ is copy {
        s[\d] *= 3;
        is $_, 'a 12', 'inside `given ... -> $_ is copy`';
    }
}

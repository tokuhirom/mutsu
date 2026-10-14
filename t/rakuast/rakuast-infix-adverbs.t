use Test;

# Adverbs on a user-declared infix in RakuAST, measured on rakudo 2026.09:
# `1 foo 3 :round` is an `ApplyInfix` whose `colonpairs` hold the written
# `ColonPair` nodes; EVAL of the tree passes them to the operator as named
# arguments, as the parsed expression does.

plan 10;

sub stmt($src, $i = 1) { $src.AST.statements[$i].expression }

# --- the tree
{
    my $a = stmt(Q[sub infix:<foo>($a, $b, :$round) { }; 1 foo 3 :round]);
    isa-ok $a, RakuAST::ApplyInfix, '`1 foo 3 :round` is an ApplyInfix';
    is $a.infix.operator, 'foo', 'over the declared operator';
    is $a.colonpairs.elems, 1, 'with one colonpair';
    isa-ok $a.colonpairs.head, RakuAST::ColonPair::True, '... the written ColonPair::True';
}
{
    my $a = stmt(Q[sub infix:<foo>($a, $b, :$x, :$y) { }; 1 foo 3 :x(5) :!y]);
    is $a.colonpairs.map(*.^name).join(','), 'RakuAST::ColonPair::Value,RakuAST::ColonPair::False',
        'several adverbs keep their order and forms';
}
{
    my $a = stmt(Q[sub infix:</>($a, $b, :$round) { }; 1 / 3 :round]);
    is $a.infix.operator, '/', 'a declared core operator spelling';
    is $a.colonpairs.elems, 1, '... with its adverb';
}

# --- EVAL of the tree
is Q[sub infix:<foo>($a, $b, :$round) { $round ?? "r$a$b" !! "n" }; 1 foo 3 :round].AST.EVAL,
    'r13', 'the adverb reaches the operator';
is Q[sub infix:</>($a, $b, :$round) { $round ?? 'round' !! 'plain' }; 1 / 3 :round].AST.EVAL,
    'round', 'the adverb reaches a declared core operator';

# --- semantics, as parsed
{
    sub infix:<bar>($a, $b, :$twice) { $twice ?? ($a + $b) * 2 !! $a + $b }
    is 1 bar 2 :twice, 6, 'the parsed expression passes the adverb';
}

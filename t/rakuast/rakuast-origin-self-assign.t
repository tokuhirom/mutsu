use Test;

# `$n = $n.abs` and `$n = $n + 1`: the assigned variable on the left is not a
# copy of the one on the right, so each keeps its own range, and the call and
# the `=` have theirs (ADR-10723 Stage 2, #15780). Ranges measured on rakudo
# 2026.09.

plan 3;

sub ranges(Str $src, *@classes) {
    my @r;
    sub walk($node) {
        my $name = $node.^name.subst('RakuAST::', '');
        if $name eq @classes.any {
            with $node.origin { @r.push: "$name {.from}..{.to}" }
        }
        $node.visit-children(&walk);
    }
    $src.AST.visit-children(&walk);
    @r.sort.join(', ')
}

is ranges('my $n = 1; $n = $n.abs;', 'Var::Lexical', 'Assignment', 'Call::Method'),
    'Assignment 14..15, Call::Method 19..22, Var::Lexical 11..13, Var::Lexical 16..18',
    'a method call on the assigned variable';
is ranges('my $n = 1; $n = $n + 1;', 'Var::Lexical', 'Assignment'),
    'Assignment 14..15, Var::Lexical 11..13, Var::Lexical 16..18',
    'an infix on the assigned variable';
is ranges('my @n = 1; @n = @n.map({$_});', 'Var::Lexical', 'Assignment'),
    'Assignment 14..15, Var::Lexical 11..13, Var::Lexical 16..18, Var::Lexical 24..26',
    'an array target';
